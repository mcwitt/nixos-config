#!/usr/bin/env python3
"""Plan and apply cleanup of stale local branches and worktrees.

`plan` is read-only apart from `git fetch --prune`. `apply` acts only on items
that a reviewed plan marked for deletion and that a fresh check still marks for
deletion with the same commit.

Requires Python >= 3.11 and Git >= 2.38. GitHub PR state is read with the
GitHub CLI when available.
"""

from __future__ import annotations

import argparse
import fnmatch
import json
import os
import re
import shutil
import subprocess
import sys
import time
import tomllib
from concurrent.futures import ThreadPoolExecutor
from dataclasses import dataclass, field
from pathlib import Path

DAY = 86400
RECENT_GIT_SECONDS = DAY
TRIVIAL_DIFF_LINES = 5
CACHE_DIRS = {
    "node_modules", ".venv", "venv", "__pycache__", ".pytest_cache", ".mypy_cache",
    ".ruff_cache", "target", "result", "build", "dist", ".tox", ".nox", ".cache",
    ".hypothesis", ".ipynb_checkpoints", ".pyrefly", ".direnv", ".eggs",
}
CACHE_SUFFIXES = (".pyc", ".pyo", ".so", ".o", ".egg-info")
WORKERS = min(16, (os.cpu_count() or 4))


# ---------------------------------------------------------------- git helpers


def run(args, cwd, input=None, check=False):
    r = subprocess.run(args, cwd=cwd, input=input, capture_output=True, text=True)
    if check and r.returncode:
        raise SystemExit(f"error: {' '.join(args)}: {r.stderr.strip()}")
    return r


class Git:
    def __init__(self, repo: Path):
        self.repo = repo

    def __call__(self, *args, cwd=None, input=None, check=False):
        return run(["git", "--no-optional-locks", *args], cwd or self.repo, input, check)

    def out(self, *args, cwd=None, input=None):
        return self(*args, cwd=cwd, input=input).stdout.strip()


# ---------------------------------------------------------------- config


@dataclass
class Config:
    horizon_days: float = 7
    remote: str = "origin"
    default_branch: str | None = None
    disposable: list[str] = field(default_factory=list)
    noise: list[str] = field(default_factory=list)
    gh: str = "gh"
    pr_limit: int = 10000
    pr_cache_minutes: float = 60


def load_config(path: Path, overrides: dict) -> Config:
    cfg = Config()
    if path.exists():
        data = tomllib.loads(path.read_text())
        for k, v in data.items():
            if not hasattr(cfg, k):
                raise SystemExit(f"error: unknown key {k!r} in {path}")
            setattr(cfg, k, v)
    for k, v in overrides.items():
        if v is not None:
            setattr(cfg, k, v)
    return cfg


def matches(rel: str, patterns: list[str]) -> bool:
    for p in patterns:
        if p.endswith("/"):
            if fnmatch.fnmatch(rel, p + "*") or fnmatch.fnmatch(rel + "/", p):
                return True
        elif fnmatch.fnmatch(rel, p) or fnmatch.fnmatch(os.path.basename(rel), p):
            return True
    return False


def is_cache(rel: str) -> bool:
    parts = rel.rstrip("/").split("/")
    return any(p in CACHE_DIRS for p in parts) or rel.rstrip("/").endswith(CACHE_SUFFIXES)


# ---------------------------------------------------------------- environment probes


def process_cwds() -> list[str]:
    """Working directories of running processes (Linux /proc, else lsof)."""
    cwds = []
    if os.path.isdir("/proc/self"):
        for pid in os.listdir("/proc"):
            if pid.isdigit():
                try:
                    cwds.append(os.readlink(f"/proc/{pid}/cwd"))
                except OSError:
                    pass
        return cwds
    if shutil.which("lsof"):
        r = run(["lsof", "-a", "-d", "cwd", "-F", "n"], "/")
        cwds = [l[1:] for l in r.stdout.splitlines() if l.startswith("n")]
    return cwds


def newest_mtime(path: Path, cap: int = 200_000) -> float:
    best, n = 0.0, 0
    try:
        best = path.lstat().st_mtime
    except OSError:
        return 0.0
    if path.is_dir() and not path.is_symlink():
        for root, dirs, files in os.walk(path):
            for f in dirs + files:
                n += 1
                if n > cap:
                    return best
                try:
                    best = max(best, os.lstat(os.path.join(root, f)).st_mtime)
                except OSError:
                    pass
    return best


def files_equal(a: str, b: str) -> bool:
    try:
        if os.path.getsize(a) != os.path.getsize(b):
            return False
        with open(a, "rb") as fa, open(b, "rb") as fb:
            while True:
                x, y = fa.read(1 << 20), fb.read(1 << 20)
                if x != y:
                    return False
                if not x:
                    return True
    except OSError:
        return False


# ---------------------------------------------------------------- survey


class Survey:
    def __init__(self, repo: Path, cfg: Config, fetch: bool, use_pr_cache: bool):
        self.git = Git(repo)
        self.cfg = cfg
        self.now = time.time()
        top = self.git("rev-parse", "--show-toplevel")
        if top.returncode:
            raise SystemExit(f"error: {repo} is not a git repository")
        self.common = Path(self.git.out("rev-parse", "--path-format=absolute", "--git-common-dir"))
        self.state = self.common / "branch-cleanup"
        self.state.mkdir(exist_ok=True)
        if fetch:
            r = self.git("fetch", "--prune", cfg.remote)
            if r.returncode:
                print(f"warning: fetch failed: {r.stderr.strip()}", file=sys.stderr)
        self.default = cfg.default_branch or self._default_branch()
        self.base = f"{cfg.remote}/{self.default}"
        if self.git("rev-parse", "--verify", "--quiet", self.base).returncode:
            raise SystemExit(f"error: {self.base} not found; set default_branch/remote")
        self.base_tree = self.git.out("rev-parse", f"{self.base}^{{tree}}")
        self.prs = self._prs(use_pr_cache)
        self.tips = self._remote_tips()
        self.tips_neg = "".join(f"^{t}\n" for t in self.tips)
        self._remote_pids: set[str] | None = None
        self.cwds = process_cwds()

    # -- remote knowledge

    def _default_branch(self) -> str:
        ref = self.git.out("symbolic-ref", "--quiet", f"refs/remotes/{self.cfg.remote}/HEAD")
        if ref:
            return ref.split("/", 3)[3]
        for name in ("main", "master"):
            if self.git("rev-parse", "--verify", "--quiet", f"{self.cfg.remote}/{name}").returncode == 0:
                return name
        raise SystemExit("error: cannot determine default branch; set default_branch")

    def _prs(self, use_cache: bool) -> list[dict]:
        cache = self.state / "prs.json"
        if use_cache and cache.exists() and self.now - cache.stat().st_mtime < self.cfg.pr_cache_minutes * 60:
            return json.loads(cache.read_text())
        if not shutil.which(self.cfg.gh.split()[0]):
            print(f"warning: {self.cfg.gh!r} not found; PR state unavailable", file=sys.stderr)
            return []
        r = run([*self.cfg.gh.split(), "pr", "list", "--state", "all", "--limit", str(self.cfg.pr_limit),
                 "--json", "number,headRefName,headRefOid,state,isCrossRepository"], self.git.repo)
        if r.returncode:
            print(f"warning: PR listing failed; PR state unavailable: {r.stderr.strip()[:300]}", file=sys.stderr)
            return []
        cache.write_text(r.stdout)
        return json.loads(r.stdout)

    def _remote_tips(self) -> list[str]:
        """Commits known to exist on the remote: remote-tracking refs, other
        fetched remote refs (such as PR heads), and PR head commits from gh."""
        globs = ["refs/remotes/"]
        for spec in self.git.out("config", "--get-all", f"remote.{self.cfg.remote}.fetch").splitlines():
            dst = spec.lstrip("+").partition(":")[2]
            if dst and not dst.startswith(("refs/remotes/", "refs/heads/", "refs/tags/")):
                globs.append(dst.split("*")[0])
        tips = set(self.git.out("for-each-ref", "--format=%(objectname)", *globs).split())
        pr_heads = [p["headRefOid"] for p in self.prs if p.get("headRefOid")]
        if pr_heads:
            r = self.git("cat-file", "--batch-check=%(objectname) %(objecttype)", input="\n".join(pr_heads) + "\n")
            tips |= {l.split()[0] for l in r.stdout.splitlines() if l.endswith(" commit")}
        return sorted(tips)

    def unpushed(self, rev: str) -> list[str]:
        return self.git.out("rev-list", rev, "--stdin", input=self.tips_neg).split()

    def patches_missing(self, commits: list[str]) -> int:
        """How many of `commits` have no patch-equivalent commit on the remote."""
        commits = [c for c in commits if len(self.git.out("rev-list", "--parents", "-1", c).split()) == 2]
        if not commits:
            return 0
        if self._remote_pids is None:
            raise RuntimeError("remote patch-ids not built")
        mine = self._patch_ids(commits)
        return sum(1 for c in commits if mine.get(c) not in self._remote_pids)

    def _patch_ids(self, commits: list[str]) -> dict[str, str]:
        log = self.git("log", "--no-walk=unsorted", "-p", "--format=commit %H", "--stdin",
                       input="\n".join(commits) + "\n").stdout
        r = self.git("patch-id", "--stable", input=log)
        out = {}
        for line in r.stdout.splitlines():
            pid, sha = line.split()
            out[sha] = pid
        return out

    def build_remote_pids(self, local_commits: set[str]):
        if self._remote_pids is not None:
            return
        self._remote_pids = set()
        if not local_commits:
            return
        dates = self.git.out("log", "--no-walk", "--format=%ct", "--stdin", input="\n".join(local_commits) + "\n")
        since = min(int(d) for d in dates.split()) - DAY
        log = self.git("log", "--no-merges", f"--since={since}", "-p", "--format=commit %H", "--stdin",
                       input="\n".join(self.tips) + "\n").stdout
        r = self.git("patch-id", "--stable", input=log)
        self._remote_pids = {l.split()[0] for l in r.stdout.splitlines()}

    # -- integration with the default branch

    def integration(self, rev: str) -> tuple[str, int | None]:
        g = self.git
        if g("merge-base", "--is-ancestor", rev, self.base).returncode == 0:
            return "ancestor", 0
        cherry = g.out("cherry", self.base, rev).split()
        if cherry and all(c == "-" for c in cherry[::2]):
            return "patch-id", 0
        mt = g("merge-tree", "--write-tree", self.base, rev)
        if mt.returncode:
            return "conflict", None
        tree = mt.stdout.split()[0]
        if tree == self.base_tree:
            return "merge-adds-nothing", 0
        lines = 0
        for l in g.out("diff", "--numstat", self.base_tree, tree).splitlines():
            a, b, _ = l.split("\t", 2)
            if a != "-":
                lines += int(a) + int(b)
        return ("trivial" if lines <= TRIVIAL_DIFF_LINES else "diverged"), lines

    # -- worktrees

    def worktrees(self) -> list[dict]:
        wts, cur = [], {}
        for line in self.git.out("worktree", "list", "--porcelain").splitlines() + [""]:
            if not line:
                if cur:
                    wts.append(cur)
                cur = {}
                continue
            k, _, v = line.partition(" ")
            cur[k] = v or True
        main = wts[0]["worktree"]
        out = []
        for w in wts:
            out.append({
                "path": w["worktree"], "head": w.get("HEAD"),
                "branch": (w.get("branch") or "").removeprefix("refs/heads/") or None,
                "locked": "locked" in w, "prunable": "prunable" in w, "main": w["worktree"] == main,
            })
        return out

    def in_use(self, path: str) -> bool:
        return any(c == path or c.startswith(path + "/") for c in self.cwds)

    def git_activity(self, path: str) -> float:
        gd = self.git.out("rev-parse", "--absolute-git-dir", cwd=path)
        t = [0.0]
        try:
            t.append(os.path.getmtime(f"{gd}/HEAD"))
            last = Path(f"{gd}/logs/HEAD").read_text().rstrip("\n").rsplit("\n", 1)[-1]
            m = re.search(r"> (\d+) [+-]\d{4}\t", last)
            if m:
                t.append(float(m.group(1)))
        except OSError:
            pass
        return max(t)

    def survey_worktree(self, w: dict, main_path: str, all_paths: list[str]) -> dict:
        path = Path(w["path"])
        nested = [q for q in all_paths if q.startswith(w["path"] + "/")]

        def inside_nested(rel: str) -> bool:
            full = str(path / rel.rstrip("/"))
            return any(full == q or full.startswith(q + "/") or q.startswith(full + "/") for q in nested)

        if w["main"] or w["prunable"] or not path.exists():
            return w
        st = self.git("status", "--porcelain=v1", "-z", "--ignored=matching", cwd=path).stdout
        dirty, ignored = [], []
        entries = st.split("\0")
        i = 0
        while i < len(entries):
            e = entries[i]
            i += 1
            if not e:
                continue
            code, rel = e[:2], e[3:]
            if code[0] in "RC":
                i += 1  # rename source follows
            if not inside_nested(rel):
                (ignored if code == "!!" else dirty).append(rel)
        w["dirty"] = dirty
        w["dirty_mtime"] = max((newest_mtime(path / r) for r in dirty), default=0.0)
        unique_kept, unique_disposable, unique_mtime = [], [], 0.0
        for ent in ignored:
            if is_cache(ent) or matches(ent, self.cfg.noise):
                continue
            root = path / ent.rstrip("/")
            walk = os.walk(root) if root.is_dir() and not root.is_symlink() else [(str(root.parent), [], [root.name])]
            for d, dirs, files in walk:
                dirs[:] = [x for x in dirs if x not in CACHE_DIRS]
                for f in files:
                    p = os.path.join(d, f)
                    rel = os.path.relpath(p, path)
                    if os.path.islink(p) or matches(rel, self.cfg.noise):
                        continue
                    if files_equal(p, os.path.join(main_path, rel)):
                        continue  # a copy of the main worktree's file
                    if matches(rel, self.cfg.disposable):
                        unique_disposable.append(rel)
                    else:
                        unique_kept.append(rel)
                        try:
                            unique_mtime = max(unique_mtime, os.lstat(p).st_mtime)
                        except OSError:
                            pass
        w["unique_ignored"] = unique_kept
        w["unique_disposable"] = len(unique_disposable)
        w["unique_mtime"] = unique_mtime
        w["git_activity"] = self.git_activity(str(path))
        w["head_date"] = int(self.git.out("log", "-1", "--format=%ct", "HEAD", cwd=path) or 0)
        w["in_use"] = self.in_use(str(path))
        w["local_commits"] = self.unpushed(w["head"]) if w["branch"] is None else []
        return w

    # -- branches

    def survey_branch(self, line: str) -> dict:
        name, sha, date, track = (line.split("\t") + [""] * 4)[:4]
        b = {"name": name, "sha": sha, "date": int(date), "gone": track == "[gone]"}
        b["local_commits"] = self.unpushed(sha)
        b["integration"], b["diff_lines"] = self.integration(sha)
        rl = self.git.out("reflog", "show", "--date=unix", "-1", "--format=%gd", f"refs/heads/{name}")
        m = re.search(r"@\{(\d+)\}", rl)
        b["activity"] = max(b["date"], int(m.group(1)) if m else 0)
        prs = sorted((p for p in self.prs if p["headRefName"] == name), key=lambda p: -p["number"])
        own = [p for p in prs if not p.get("isCrossRepository")] or prs
        if own:
            p = own[0]
            b["pr"], b["pr_state"] = p["number"], p["state"]
            b["beyond_pr"] = self.git("merge-base", "--is-ancestor", sha, p["headRefOid"]).returncode != 0
        return b


# ---------------------------------------------------------------- planning


def plan(sv: Survey) -> dict:
    cfg, now = sv.cfg, sv.now
    horizon = cfg.horizon_days * DAY
    wts = sv.worktrees()
    main_path = next(w["path"] for w in wts if w["main"])
    with ThreadPoolExecutor(WORKERS) as ex:
        paths = [w["path"] for w in wts]
        wts = list(ex.map(lambda w: sv.survey_worktree(w, main_path, paths), wts))
        refs = sv.git.out("for-each-ref", "refs/heads",
                          "--format=%(refname:short)\t%(objectname)\t%(committerdate:unix)\t%(upstream:track)").splitlines()
        brs = list(ex.map(sv.survey_branch, refs))

    sv.build_remote_pids({c for x in wts + brs for c in x.get("local_commits", [])})
    with ThreadPoolExecutor(WORKERS) as ex:
        for x, n in zip(wts + brs, ex.map(lambda x: sv.patches_missing(x.get("local_commits", [])), wts + brs)):
            x["lost_commits"] = n

    for w in wts:
        a = r = None
        if w["main"]:
            a, r = "keep", "main worktree"
        elif w["prunable"] or not Path(w["path"]).exists():
            a, r = "prune", "directory missing"
        elif w["locked"]:
            a, r = "keep", "locked"
        elif w["in_use"]:
            a, r = "keep", "in use by a running process"
        elif now - w["git_activity"] < RECENT_GIT_SECONDS:
            a, r = "keep", "git activity in the last 24h"
        else:
            risky = []
            if w["dirty"]:
                risky.append(f"{len(w['dirty'])} uncommitted path(s)")
            if w["lost_commits"]:
                risky.append(f"detached with {w['lost_commits']} commit(s) not on the remote")
            if w["unique_ignored"]:
                risky.append(f"{len(w['unique_ignored'])} unique ignored file(s)")
            note = f"; {w['unique_disposable']} disposable file(s)" if w["unique_disposable"] else ""
            if not risky:
                a, r = "delete", "clean" + note
            else:
                last = max(w["git_activity"], w["head_date"], w["dirty_mtime"], w["unique_mtime"])
                age = (now - last) / DAY
                a = "delete" if now - last >= horizon else "keep"
                r = f"{', '.join(risky)}; last activity {age:.1f}d ago" + note
        w["action"], w["reason"] = a, r

    # A worktree nested inside another cannot outlive its parent's removal.
    changed = True
    while changed:
        changed = False
        for p in wts:
            if p["action"] == "delete" and any(
                    c["action"] != "delete" and c["path"].startswith(p["path"] + "/") for c in wts):
                p["action"], p["reason"], changed = "keep", "contains a kept worktree", True

    kept_branches = {w["branch"] for w in wts if w["branch"] and w["action"] != "delete"}
    for b in brs:
        reasons = []
        if b["integration"] in ("ancestor", "patch-id", "merge-adds-nothing"):
            reasons.append(f"integrated ({b['integration']})")
        elif b["integration"] == "trivial":
            reasons.append(f"trivial diff vs {sv.base} ({b['diff_lines']} lines)")
        if b.get("pr_state") == "MERGED" and not b.get("beyond_pr"):
            reasons.append(f"PR #{b['pr']} merged")
        elif b.get("pr_state") == "CLOSED":
            reasons.append(f"PR #{b['pr']} closed")
        if b["gone"]:
            reasons.append("upstream deleted")
        age = (now - b["date"]) / DAY
        if now - b["date"] >= horizon and b.get("pr_state") != "OPEN":
            reasons.append(f"no commits in {age:.0f}d")
        idle = now - b["activity"] >= horizon
        if b["name"] == sv.default:
            a, r = "keep", "default branch"
        elif b["name"] in kept_branches:
            a, r = "keep", "checked out in a kept worktree"
        elif not reasons:
            a, r = "keep", "active" + (f", PR #{b['pr']} {b['pr_state'].lower()}" if b.get("pr") else "")
        elif b["lost_commits"] and not idle:
            a, r = "keep", f"{b['lost_commits']} local-only commit(s), active {(now - b['activity']) / DAY:.1f}d ago"
        else:
            a = "delete"
            r = ", ".join(reasons) + (f"; loses {b['lost_commits']} local-only commit(s)" if b["lost_commits"] else "")
        b["action"], b["reason"] = a, r

    return {
        "repo": str(sv.git.repo), "created": now, "horizon_days": cfg.horizon_days, "base": sv.base,
        "worktrees": [{k: w.get(k) for k in ("path", "branch", "head", "action", "reason", "lost_commits")} for w in wts],
        "branches": [{k: b.get(k) for k in ("name", "sha", "action", "reason", "lost_commits", "pr", "pr_state")} for b in brs],
    }


def summarize(p: dict, verbose: bool) -> str:
    out = [f"Repository {p['repo']} (base {p['base']}, horizon {p['horizon_days']}d)"]
    for kind, key, label in (("worktrees", "path", "Worktrees"), ("branches", "name", "Branches")):
        items = p[kind]
        counts = {}
        for x in items:
            counts[x["action"]] = counts.get(x["action"], 0) + 1
        out.append(f"\n{label}: " + ", ".join(f"{v} {k}" for k, v in sorted(counts.items())))
        for action in ("delete", "prune", "keep"):
            group = sorted((x for x in items if x["action"] == action), key=lambda x: x[key])
            if not group or (action == "keep" and not verbose and kind == "branches"):
                continue
            out.append(f"  {action}:")
            out += [f"    {x[key]}" + (f" [{x['branch'] or 'detached'}]" if kind == "worktrees" else "")
                    + f" — {x['reason']}" for x in group]
    lost = sum(x["lost_commits"] or 0 for x in p["worktrees"] + p["branches"] if x["action"] == "delete")
    out.append(f"\nDeleting the planned items loses {lost} commit(s) that exist only locally.")
    return "\n".join(out)


# ---------------------------------------------------------------- apply


def apply(plan_path: Path, args) -> int:
    reviewed = json.loads(plan_path.read_text())
    repo = Path(reviewed["repo"])
    cfg = load_config(config_path(repo), {"horizon_days": reviewed["horizon_days"]})
    git = Git(repo)
    common = Path(git.out("rev-parse", "--path-format=absolute", "--git-common-dir"))
    log_path = common / "branch-cleanup" / time.strftime("deleted-%Y%m%d-%H%M%S.tsv")
    log = []

    fresh = plan(Survey(repo, cfg, fetch=False, use_pr_cache=True))
    want = {w["path"]: w["head"] for w in reviewed["worktrees"] if w["action"] == "delete"}
    now_ok = {w["path"]: w for w in fresh["worktrees"] if w["action"] == "delete"}
    todo, skipped = [], []
    for path, head in want.items():
        w = now_ok.get(path)
        if w and w["head"] == head:
            todo.append(w)
        else:
            skipped.append((path, "no longer eligible" if not w else "HEAD moved"))
    removed = 0
    for w in sorted(todo, key=lambda w: -len(w["path"])):  # nested worktrees first
        r = git("worktree", "remove", "--force", w["path"])
        if r.returncode:
            skipped.append((w["path"], r.stderr.strip()[:200]))
        else:
            removed += 1
            log.append(("worktree", w["path"], w["head"] or "", w["branch"] or ""))
    git("worktree", "prune")
    pruned = sum(1 for w in reviewed["worktrees"] if w["action"] == "prune")

    fresh = plan(Survey(repo, cfg, fetch=False, use_pr_cache=True))
    want_b = {b["name"]: b["sha"] for b in reviewed["branches"] if b["action"] == "delete"}
    # Branches freed by the worktree removals above were planned with the same rules.
    ok_b = {b["name"]: b["sha"] for b in fresh["branches"] if b["action"] == "delete"}
    names = [n for n, sha in want_b.items() if ok_b.get(n) == sha]
    skipped += [(n, "no longer eligible or moved") for n in want_b if n not in names]
    deleted = 0
    for i in range(0, len(names), 100):
        chunk = names[i:i + 100]
        r = git("branch", "-D", *chunk)
        if r.returncode:
            print(r.stderr.strip(), file=sys.stderr)
        for n in chunk:
            if git("rev-parse", "--verify", "--quiet", f"refs/heads/{n}").returncode:
                deleted += 1
                log.append(("branch", n, want_b[n], ""))
    log_path.write_text("".join("\t".join(x) + "\n" for x in log))
    print(f"Removed {removed} worktree(s), pruned {pruned} missing entr(ies), deleted {deleted} branch(es).")
    print(f"Log (restore a branch with `git branch <name> <sha>`): {log_path}")
    if skipped:
        print(f"Skipped {len(skipped)}:")
        for x, why in skipped:
            print(f"  {x} — {why}")
    return 0


# ---------------------------------------------------------------- CLI


def config_path(repo: Path) -> Path:
    common = run(["git", "rev-parse", "--path-format=absolute", "--git-common-dir"], repo, check=True).stdout.strip()
    return Path(common) / "info" / "branch-cleanup.toml"


def main(argv=None) -> int:
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    sub = ap.add_subparsers(dest="cmd", required=True)
    pp = sub.add_parser("plan", help="survey and write a plan (read-only apart from fetch)")
    pp.add_argument("repo", nargs="?", default=".")
    pp.add_argument("--horizon", type=float, dest="horizon_days", help="inactivity horizon in days (default 7)")
    pp.add_argument("--remote")
    pp.add_argument("--default-branch")
    pp.add_argument("--disposable", action="append", help="ignored-file glob removable regardless of age (repeatable)")
    pp.add_argument("--gh", help="GitHub CLI command (default: gh)")
    pp.add_argument("--no-fetch", action="store_true")
    pp.add_argument("--refresh-prs", action="store_true", help="ignore the cached PR list")
    pp.add_argument("--out", type=Path, help="plan path (default: <git-common-dir>/branch-cleanup/plan.json)")
    pp.add_argument("--verbose", "-v", action="store_true", help="also list kept branches")
    ap_ = sub.add_parser("apply", help="delete what a reviewed plan marked and a fresh check confirms")
    ap_.add_argument("plan", type=Path)
    args = ap.parse_args(argv)

    if args.cmd == "apply":
        return apply(args.plan, args)

    repo = Path(args.repo).resolve()
    overrides = {k: getattr(args, k) for k in ("horizon_days", "remote", "default_branch", "gh")}
    cfg = load_config(config_path(repo), overrides)
    if args.disposable:
        cfg.disposable = cfg.disposable + args.disposable
    sv = Survey(repo, cfg, fetch=not args.no_fetch, use_pr_cache=not args.refresh_prs)
    p = plan(sv)
    out = args.out or sv.state / "plan.json"
    out.write_text(json.dumps(p, indent=1))
    print(summarize(p, args.verbose))
    print(f"\nPlan written to {out}")
    return 0


if __name__ == "__main__":
    sys.exit(main())
