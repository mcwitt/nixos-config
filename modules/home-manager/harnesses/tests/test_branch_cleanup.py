"""Tests for the branch-cleanup skill script against a synthetic repository.

Usage: python3 test_branch_cleanup.py <path/to/branch_cleanup.py>
"""

import json
import os
import subprocess
import sys
import tempfile
import time
import unittest
from pathlib import Path

SCRIPT = Path(sys.argv.pop(1)).resolve() if len(sys.argv) > 1 else None
DAY = 86400
OLD = int(time.time()) - 30 * DAY


def sh(*args, cwd, date=None, check=True):
    env = dict(os.environ)
    if date is not None:
        env["GIT_AUTHOR_DATE"] = env["GIT_COMMITTER_DATE"] = f"@{date} +0000"
    r = subprocess.run(args, cwd=cwd, env=env, capture_output=True, text=True)
    if check and r.returncode:
        raise AssertionError(f"{args}: {r.stderr}")
    return r.stdout.strip()


def age(path: Path, when: int):
    """Backdate a worktree's files and its git-dir HEAD."""
    gd = Path(sh("git", "rev-parse", "--absolute-git-dir", cwd=path))
    for p in [gd / "HEAD", *path.rglob("*")]:
        if ".git" not in p.relative_to(path if p.is_relative_to(path) else p.parent).parts:
            os.utime(p, (when, when), follow_symlinks=False)


class BranchCleanupTest(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.tmp = Path(tempfile.mkdtemp())
        os.environ.update(HOME=str(cls.tmp), GIT_CONFIG_NOSYSTEM="1", GIT_CONFIG_GLOBAL=str(cls.tmp / "gitconfig"))
        t = cls.tmp
        sh("git", "config", "--global", "user.name", "t", cwd=t)
        sh("git", "config", "--global", "user.email", "t@example.com", cwd=t)
        sh("git", "config", "--global", "init.defaultBranch", "main", cwd=t)
        sh("git", "init", "--bare", "remote.git", cwd=t)
        sh("git", "clone", "remote.git", "repo", cwd=t)
        r = cls.repo = t / "repo"

        def commit(msg, f, content, date=OLD, cwd=r):
            (cwd / f).write_text(content)
            sh("git", "add", f, cwd=cwd)
            sh("git", "commit", "-m", msg, cwd=cwd, date=date)

        (r / ".gitignore").write_text("logs/\nscratch/\nshared.dat\n")
        sh("git", "add", ".gitignore", cwd=r)
        commit("base", "a.txt", "a\n")
        (r / "shared.dat").write_text("same everywhere\n")
        sh("git", "push", "-u", "origin", "main", cwd=r)

        # merged: an ancestor of main.
        sh("git", "switch", "-c", "merged", cwd=r, date=OLD)
        commit("m", "m.txt", "m\n")
        sh("git", "switch", "main", cwd=r)
        sh("git", "merge", "--ff-only", "merged", cwd=r, date=OLD)
        # squashed: same content landed on main as a different commit.
        sh("git", "switch", "-c", "squashed", cwd=r, date=OLD)
        commit("s1", "s.txt", "s1\n")
        commit("s2", "s.txt", "s2\n")
        sh("git", "switch", "main", cwd=r)
        commit("squash s", "s.txt", "s2\n")
        sh("git", "push", "origin", "main", cwd=r)
        # old-local: unpushed work, idle past the horizon.
        sh("git", "switch", "-c", "old-local", "main", cwd=r, date=OLD)
        commit("ol", "ol.txt", "ol\n")
        # fresh-local: unpushed work committed now.
        sh("git", "switch", "-c", "fresh-local", "main", cwd=r)
        commit("fl", "fl.txt", "fl\n", date=int(time.time()))
        # rebased: local commits whose patches are on the remote under other SHAs.
        sh("git", "switch", "-c", "rebased", "main", cwd=r, date=OLD)
        commit("rb", "rb.txt", "rb\n")
        sh("git", "push", "origin", "rebased:pushed-copy", cwd=r)
        sh("git", "commit", "--amend", "--no-edit", "--date", f"@{OLD + 5}", cwd=r, date=OLD + 5)
        sh("git", "switch", "main", cwd=r)

        def wt(name, branch=None, detach=False, when=OLD):
            p = t / name
            args = ["git", "worktree", "add"] + (["--detach", str(p), "main"] if detach else ["-b", branch, str(p), "main"])
            sh(*args, cwd=r, date=when)
            return p

        cls.wt_clean = wt("wt-clean", "wt-clean-branch")
        (cls.wt_clean / "shared.dat").write_text("same everywhere\n")  # copied from main
        cls.wt_dirty_new = wt("wt-dirty-new", "wt-dirty-new-branch")
        cls.wt_dirty_old = wt("wt-dirty-old", "wt-dirty-old-branch")
        cls.wt_unique = wt("wt-unique", "wt-unique-branch")
        cls.wt_logs = wt("wt-logs", "wt-logs-branch")
        cls.wt_busy = wt("wt-busy", "wt-busy-branch")
        cls.wt_parent = wt("wt-parent", "wt-parent-branch")
        cls.wt_detached = wt("wt-detached", detach=True)
        commit("det", "det.txt", "det\n", cwd=cls.wt_detached)
        (cls.wt_dirty_new / "a.txt").write_text("edited\n")
        (cls.wt_dirty_old / "a.txt").write_text("edited\n")
        (cls.wt_unique / "scratch").mkdir()
        (cls.wt_unique / "scratch" / "result.csv").write_text("1\n")
        (cls.wt_logs / "logs").mkdir()
        (cls.wt_logs / "logs" / "run.log").write_text("x\n")
        for p in (cls.wt_clean, cls.wt_dirty_old, cls.wt_logs, cls.wt_busy, cls.wt_parent, cls.wt_detached, cls.wt_unique):
            age(p, OLD)
        # wt-unique keeps a fresh unique ignored file and wt-dirty-new a fresh edit.
        os.utime(cls.wt_unique / "scratch" / "result.csv")
        gd = Path(sh("git", "rev-parse", "--absolute-git-dir", cwd=cls.wt_dirty_new))
        os.utime(gd / "HEAD", (OLD, OLD))
        nested = cls.wt_parent / "nested"
        sh("git", "worktree", "add", "-b", "nested-branch", str(nested), "main", cwd=r)  # recent: kept
        cls.busy = subprocess.Popen(["sleep", "300"], cwd=cls.wt_busy)
        (r / ".git" / "info" / "branch-cleanup.toml").write_text('disposable = ["logs/"]\ngh = "no-such-gh"\n')

        cls.plan_path = t / "plan.json"
        out = sh(sys.executable, str(SCRIPT), "plan", str(r), "--out", str(cls.plan_path), cwd=t)
        cls.summary = out
        cls.plan = json.loads(cls.plan_path.read_text())
        cls.wts = {Path(w["path"]).name: w for w in cls.plan["worktrees"]}
        cls.brs = {b["name"]: b for b in cls.plan["branches"]}

    @classmethod
    def tearDownClass(cls):
        cls.busy.kill()
        cls.busy.wait()

    def wt(self, name):
        return self.wts[name]["action"], self.wts[name]["reason"]

    def br(self, name):
        return self.brs[name]["action"], self.brs[name]["reason"]

    def test_worktrees(self):
        self.assertEqual(self.wt("repo")[0], "keep")
        self.assertEqual(self.wt("wt-clean"), ("delete", "clean"))
        self.assertEqual(self.wt("wt-dirty-new")[0], "keep")
        self.assertEqual(self.wt("wt-dirty-old")[0], "delete")
        self.assertEqual(self.wt("wt-unique")[0], "keep")
        self.assertIn("unique ignored", self.wt("wt-unique")[1])
        self.assertEqual(self.wt("wt-logs")[0], "delete")
        self.assertIn("disposable", self.wt("wt-logs")[1])
        self.assertEqual(self.wt("wt-busy"), ("keep", "in use by a running process"))
        self.assertEqual(self.wt("nested"), ("keep", "git activity in the last 24h"))
        self.assertEqual(self.wt("wt-parent"), ("keep", "contains a kept worktree"))
        self.assertEqual(self.wt("wt-detached")[0], "delete")
        self.assertEqual(self.wts["wt-detached"]["lost_commits"], 1)

    def test_branches(self):
        self.assertEqual(self.br("main")[0], "keep")
        self.assertEqual(self.br("merged")[0], "delete")
        self.assertIn("integrated", self.br("squashed")[1])
        self.assertEqual(self.br("old-local")[0], "delete")
        self.assertEqual(self.brs["old-local"]["lost_commits"], 1)
        self.assertEqual(self.br("fresh-local")[0], "keep")
        self.assertEqual(self.br("rebased")[0], "delete")
        self.assertEqual(self.brs["rebased"]["lost_commits"], 0)
        # Freed by a planned worktree removal vs. still checked out.
        self.assertEqual(self.br("wt-clean-branch")[0], "delete")
        self.assertEqual(self.br("wt-busy-branch"), ("keep", "checked out in a kept worktree"))

    def test_zz_apply(self):
        # A worktree that becomes busy after review is skipped.
        late = subprocess.Popen(["sleep", "300"], cwd=self.wt_dirty_old)
        try:
            out = sh(sys.executable, str(SCRIPT), "apply", str(self.plan_path), cwd=self.tmp)
        finally:
            late.kill()
            late.wait()
        self.assertFalse(self.wt_clean.exists())
        self.assertFalse(self.wt_logs.exists())
        self.assertTrue(self.wt_dirty_old.exists())
        self.assertTrue(self.wt_busy.exists())
        heads = sh("git", "for-each-ref", "--format=%(refname:short)", "refs/heads", cwd=self.repo).split()
        for gone in ("merged", "squashed", "old-local", "rebased", "wt-clean-branch"):
            self.assertNotIn(gone, heads)
        for kept in ("main", "fresh-local", "wt-busy-branch", "wt-dirty-old-branch"):
            self.assertIn(kept, heads)
        self.assertIn("Skipped", out)
        log = next((self.repo / ".git" / "branch-cleanup").glob("deleted-*.tsv")).read_text()
        self.assertIn("old-local", log)


if __name__ == "__main__":
    unittest.main(verbosity=2)
