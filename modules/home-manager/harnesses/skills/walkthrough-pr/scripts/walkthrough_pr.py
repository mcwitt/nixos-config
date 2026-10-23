#!/usr/bin/env python3
"""Check and serve a slideshow that walks a reader through a code change.

This is the teaching slideshow engine (walkthrough's deck.py, which it imports
from the skill installed beside it) with a review layer. A deck names unified
diffs of the change, shows changed lines in `diff` blocks, and must show
every changed line on some slide or appendix group. The engine ends the deck
with a Findings slide, where the reader collects and triages findings, and a
Proposed actions slide, where the session drafts review comments, fixes and
checks that run once the reader approves them.

`hunks` lists the hunks of a deck's diffs with the ids slides refer to.
`finding` and `action` let the session record findings and propose actions.
The other commands are deck.py's. The server keeps findings and actions in
`findings.json`; the session's edits reach it as `queue/op-*.json` files.
"""

from __future__ import annotations

import codecs
import contextlib
import functools
import json
import re
import sys
import threading
import time
from collections import defaultdict
from dataclasses import dataclass
from pathlib import Path

CORE = str(Path(__file__).resolve().parent.parent.parent / "walkthrough" / "scripts")
if CORE not in sys.path:
    sys.path.insert(0, CORE)
import deck as core  # noqa: E402
from deck import (  # noqa: E402
    CLAIM_STALE, Deck, Extension, FileDiff, Hunk, Listening, Loading, Queue, Row, _str, fenced, file_payload, log,
    new_id, parse_refs, read_json, row_span, server_info, stale_reason, write_atomic,
)

FINDING_ID = re.compile(r"^f-[0-9a-z-]+$")
SEVERITIES = ("P0", "P1", "P2", "P3")
FINDING_ACTIONS = ("ignore", "comment")
ACTION_ID = re.compile(r"^a-[0-9a-z-]+$")
ACTION_KINDS = ("comment", "fix", "check")
PR_URL = re.compile(r"^https://github\.com/[\w.-]+/[\w.-]+/pull/\d+$")
HUNK_RE = re.compile(r"^@@ -(\d+)(?:,(\d+))? \+(\d+)(?:,(\d+))? @@ ?(.*)$")
RANGE_RE = re.compile(r"^(new|old|rows):(\d+)(?:-(\d+))?$")
APPENDIX_ROWS = 160  # rows per appendix slide before groups spill onto the next
UNCOVERED_GAP = 6  # uncovered runs closer than this are reported together
# Slides the engine adds to a deck with diffs, after the appendix.
FINDINGS_SLIDE = {"id": "findings", "title": "Findings", "section": "Review", "generated": "findings",
                  "prompts": [], "blocks": [{"type": "findings"}], "quiz": []}
ACTIONS_SLIDE = {"id": "proposed-actions", "title": "Proposed actions", "section": "Review", "generated": "actions",
                 "prompts": [], "blocks": [{"type": "actions"}], "quiz": []}


class DiffError(ValueError):
    pass


def _unquote(path: str) -> str:
    if len(path) >= 2 and path[0] == '"' and path[-1] == '"':
        return codecs.escape_decode(path[1:-1].encode())[0].decode("utf-8", "replace")
    return path


def _side(path: str, prefix: str) -> str | None:
    path = _unquote(path.split("\t", 1)[0].rstrip("\n"))
    if path == "/dev/null":
        return None
    return path[len(prefix) :] if path.startswith(prefix) else path


def _git_header_paths(rest: str) -> tuple[str | None, str | None]:
    """Paths from `diff --git a/X b/Y`, used when no ---/+++ lines follow."""
    if rest.startswith('"'):
        end = 1
        while end < len(rest) and (rest[end] != '"' or rest[end - 1] == "\\"):
            end += 1
        old, new = rest[: end + 1], rest[end + 1 :].strip()
    elif rest.endswith('"'):
        start = rest.index(' "')
        old, new = rest[:start], rest[start + 1 :]
    else:
        half = (len(rest) - 1) // 2
        old, new = rest[:half], rest[half + 1 :]
    return _side(old, "a/"), _side(new, "b/")


def parse_diff(text: str, label: str = "") -> list[FileDiff]:
    """Parse `git diff` output (or plain `diff -u` output) into files and hunks."""
    files: list[FileDiff] = []
    lines = text.splitlines()
    i = 0
    cur: FileDiff | None = None

    def start(old: str | None, new: str | None) -> FileDiff:
        f = FileDiff(key="", label=label, old_path=old, new_path=new)
        files.append(f)
        return f

    while i < len(lines):
        line = lines[i]
        if line.startswith("diff --cc ") or line.startswith("diff --combined "):
            raise DiffError("combined (merge) diffs are not supported; diff against one parent")
        if line.startswith("diff --git "):
            cur = start(*_git_header_paths(line[len("diff --git ") :]))
            i += 1
            continue
        if line.startswith("--- ") and i + 1 < len(lines) and lines[i + 1].startswith("+++ "):
            old, new = _side(line[4:], "a/"), _side(lines[i + 1][4:], "b/")
            if cur is None or cur.hunks or cur.binary:
                cur = start(old, new)
            else:
                cur.old_path, cur.new_path = old, new
            if old is None:
                cur.status = "added"
            elif new is None:
                cur.status = "deleted"
            i += 2
            continue
        if cur is None:
            i += 1
            continue
        m = HUNK_RE.match(line)
        if m:
            os_, oc, ns, nc = (int(m[1]), int(m[2] or 1), int(m[3]), int(m[4] or 1))
            h = Hunk("", cur, line, m[5], os_, oc, ns, nc)
            o, n = os_, ns
            left_old, left_new = oc, nc
            i += 1
            while (left_old > 0 or left_new > 0) and i < len(lines):
                body = lines[i]
                if body.startswith("\\"):
                    if h.rows:
                        h.rows[-1].noeol = True
                    i += 1
                    continue
                kind = body[:1] or " "
                if kind == " " and left_old > 0 and left_new > 0:
                    h.rows.append(Row(" ", o, n, body[1:]))
                    o, n, left_old, left_new = o + 1, n + 1, left_old - 1, left_new - 1
                elif kind == "-" and left_old > 0:
                    h.rows.append(Row("-", o, None, body[1:]))
                    o, left_old = o + 1, left_old - 1
                elif kind == "+" and left_new > 0:
                    h.rows.append(Row("+", None, n, body[1:]))
                    n, left_new = n + 1, left_new - 1
                else:
                    raise DiffError(f"{cur.path}: malformed hunk near line {i + 1}: {body[:60]!r}")
                i += 1
            if i < len(lines) and lines[i].startswith("\\") and h.rows:
                h.rows[-1].noeol = True
                i += 1
            cur.hunks.append(h)
            continue
        if line.startswith("new file mode"):
            cur.status = "added"
        elif line.startswith("deleted file mode"):
            cur.status = "deleted"
        elif line.startswith("rename from "):
            cur.status, cur.old_path = "renamed", _unquote(line[len("rename from ") :])
        elif line.startswith("rename to "):
            cur.new_path = _unquote(line[len("rename to ") :])
        elif line.startswith("copy from "):
            cur.status, cur.old_path = "copied", _unquote(line[len("copy from ") :])
        elif line.startswith("copy to "):
            cur.new_path = _unquote(line[len("copy to ") :])
        elif line.startswith("Binary files ") or line.startswith("GIT binary patch"):
            cur.binary = True
        elif line.startswith(("old mode", "new mode")) and cur.status == "modified":
            cur.status = "mode changed"
        if line.startswith(("old mode", "new mode", "similarity index", "dissimilarity index")):
            cur.meta.append(line)
        i += 1

    seen: dict[str, int] = defaultdict(int)
    for f in files:
        base = f"{label}:{f.path}" if label else f.path
        seen[base] += 1
        f.key = base if seen[base] == 1 else f"{base}~{seen[base]}"
        for n, h in enumerate(f.hunks, 1):
            h.id = f"{f.key}#{n}"
    return files


def resolve_range(h: Hunk, spec: str | None) -> tuple[int, int]:
    """Row indices (inclusive) selected by `new:A-B`, `old:A-B` or `rows:A-B`."""
    if not spec:
        return 0, len(h.rows) - 1
    m = RANGE_RE.match(spec.replace(" ", ""))
    if not m:
        raise ValueError(f"range {spec!r} is not new:A-B, old:A-B or rows:A-B")
    side, a, b = m[1], int(m[2]), int(m[3] or m[2])
    if side == "rows":
        if not (1 <= a <= b <= len(h.rows)):
            raise ValueError(f"range {spec!r} is outside rows 1-{len(h.rows)}")
        return a - 1, b - 1
    hits = [i for i, r in enumerate(h.rows) if (getattr(r, side) or 0) and a <= getattr(r, side) <= b]
    if not hits:
        raise ValueError(f"range {spec!r} matches no rows of {h.id} ({row_span(h.rows, 0, len(h.rows) - 1)})")
    lo, hi = hits[0], hits[-1]
    # Keep both halves of a replacement: removed rows directly before the
    # first new-side row, or added rows directly after the last old-side row.
    if side == "new":
        while lo > 0 and h.rows[lo - 1].kind == "-":
            lo -= 1
    else:
        while hi + 1 < len(h.rows) and h.rows[hi + 1].kind == "+":
            hi += 1
    return lo, hi


def changed(h: Hunk) -> set[int]:
    return {i for i, r in enumerate(h.rows) if r.kind != " "}


def needed(h: Hunk) -> set[int]:
    """Changed rows that coverage requires: blank lines carry nothing to show."""
    return {i for i in changed(h) if h.rows[i].text.strip()}


def runs(indices: set[int], gap: int = 1) -> list[tuple[int, int]]:
    """Group row indices into (first, last) runs, joining those at most `gap` apart."""
    out: list[list[int]] = []
    for i in sorted(indices):
        if out and i - out[-1][1] <= gap:
            out[-1][1] = i
        else:
            out.append([i, i])
    return [(lo, hi) for lo, hi in out]


def changed_spans(h: Hunk, indices: set[int]) -> list[str]:
    return [row_span(h.rows, lo, hi) for lo, hi in runs(indices)]


def _excerpt(blk: dict, where: str, files: dict[str, FileDiff], hunks: dict[str, Hunk],
             errors: list[str]) -> dict | None:
    """Resolve {"diff": file-or-hunk, "range": ...} to per-hunk row spans."""
    target, spec = blk["diff"], blk.get("range")
    if target in hunks:
        h = hunks[target]
        try:
            lo, hi = resolve_range(h, spec)
        except ValueError as e:
            errors.append(f"{where}: {e}")
            return None
        return {"file": h.file.key, "pieces": [{"hunk": h.id, "lo": lo, "hi": hi}]}
    f = files.get(target)
    if f is None:
        errors.append(f"{where}: {target!r} is not a file or hunk in the diffs (see `hunks`)")
        return None
    if spec and spec.startswith("rows:"):
        errors.append(f"{where}: rows: ranges need a hunk id, not a file")
        return None
    pieces = []
    for h in f.hunks:
        try:
            lo, hi = resolve_range(h, spec)
        except ValueError:
            continue
        pieces.append({"hunk": h.id, "lo": lo, "hi": hi})
    if f.hunks and not pieces:
        spans = ", ".join(row_span(h.rows, 0, len(h.rows) - 1) for h in f.hunks)
        errors.append(f"{where}: range {spec!r} matches no rows of {target} (hunks cover {spans})")
        return None
    return {"file": f.key, "pieces": pieces}


def _uncovered(h: Hunk, shown: set[int]) -> list[tuple[int, int]]:
    return runs(needed(h) - shown, UNCOVERED_GAP)


def _appendix_slides(groups: list[dict]) -> list[dict]:
    """Lay out appendix groups as slides, keeping each group on one slide."""
    pages: list[list[dict]] = [[]]
    used = 0
    for g in groups:
        rows = sum(p["hi"] - p["lo"] + 1 for b in g["blocks"] for p in b["pieces"]) or 1
        if pages[-1] and used + rows > APPENDIX_ROWS:
            pages.append([])
            used = 0
        pages[-1].append(g)
        used += rows
    out = []
    for n, page in enumerate(pages, 1):
        # The section label already says "Appendix".
        titles = [g["title"] for g in page]
        joined = ", ".join(titles)
        title = joined if len(joined) <= 70 else f"{titles[0]} and {len(titles) - 1} more"
        blocks = [{"type": "group", "title": g["title"], "md": g["note"], "blocks": g["blocks"]} for g in page]
        out.append({"id": f"appendix-{n}", "title": title, "section": "Appendix",
                    "prompts": [], "blocks": blocks, "quiz": []})
    return out


def place_key(r: dict) -> tuple:
    """What makes two references the same place: a quote is its text."""
    return (r.get("path"), r.get("side"), r.get("line"), r.get("end")) if "path" in r else (r.get("text"),)


def locate(deck: Deck, spec: str) -> dict:
    """A reference to PATH:LINE[-END], new-side line numbers: the diff's lines when it shows them."""
    m = re.fullmatch(r"(.+):(\d+)(?:-(\d+))?", spec)
    if not m:
        raise ValueError(f"{spec}: expected PATH:LINE or PATH:LINE-END")
    path, line = m[1], int(m[2])
    end = max(int(m[3] or line), line)
    for h in (h for f in deck.files if not f.plain and path in (f.key, f.path) for h in f.hunks):
        rows = [i for i, r in enumerate(h.rows) if r.new is not None and line <= r.new <= end]
        if rows:
            return parse_refs(deck.hunks, [{"kind": "lines", "hunk": h.id, "lo": rows[0], "hi": rows[-1]}])[0]
    return parse_refs(deck.hunks, [{"kind": "place", "path": path, "line": line, "end": end}])[0]


@dataclass
class OpContext:
    """What an edit to the review sees: the deck and the hunks a finding may point into."""
    deck: Deck
    hunks: dict[str, Hunk]


class Review:
    """A review deck's findings and proposed actions, persisted to findings.json.

    A finding is {id, text, author, severity, sources, refs, slide, at,
    action, hidden, merged}: author wrote its current text, "session" or
    "reader"; severity is P0 to P3 or null, sources lists "reader" and
    "review", action is the reader's choice (ignore or comment), and merged
    holds the findings folded into it. The background review's findings stay
    hidden from the page until the reader asks to see them: `reveal` goes
    from hidden to requested (the session merges duplicates) to shown.
    `review` is the background review's status: none, running or done.

    A proposed action is {id, kind, title, finding, include, state, result}
    plus, for a comment, body, author and place; for a fix, patch (a unified
    diff); for a check, command. A comment's author is whoever wrote its
    current text, "session" or "reader", and decides whose account posts it;
    a comment without a place goes in the review's summary. state goes from
    proposed to running to done or failed. `proposal` is none, requested or
    ready.

    Only the server writes it, through op(): edits come from the page (by
    "reader") and from the `finding` and `action` commands (by "session")."""

    OPS = {
        "reader": {"add", "set", "remove", "request-reveal", "request-proposal", "action-set", "carry-out"},
        "session": {"add", "set", "remove", "merge", "review", "reveal",
                    "action-add", "action-set", "action-remove", "action-ready", "action-result"},
    }

    def __init__(self, path: Path, queue: Queue):
        self.path = path
        self.queue = queue
        self.lock = threading.Lock()
        data = read_json(path)
        self.items: list[dict] = data.get("findings", [])
        for f in self.items:
            f.setdefault("hidden", False)
            f.setdefault("merged", [])
            f.setdefault("author", "session")  # older files did not record it; the agent's account is the safe guess
        self.actions: list[dict] = data.get("actions", [])
        self.review: str = data.get("review", "none")
        self.reveal: str = data.get("reveal", "hidden")
        self.proposal: str = data.get("proposal", "none")

    def save(self):
        data = {"version": 1, "review": self.review, "reveal": self.reveal, "proposal": self.proposal,
                "findings": self.items, "actions": self.actions}
        write_atomic(self.path, json.dumps(data, indent=1))

    def get(self, fid: str) -> dict | None:
        return next((f for f in self.items if f["id"] == fid), None)

    def snapshot(self) -> dict:
        """What the page shows: everything but the hidden findings, which it only counts."""
        with self.lock:
            shown = [f for f in self.items if not f.get("hidden")]
            actions = [{**a, **({"diff": fix_payload(a)} if a["kind"] == "fix" else {}), "resend_in": resend_in(a)}
                       for a in self.actions]
            return json.loads(json.dumps({"findings": shown, "hidden": len(self.items) - len(shown), "review": self.review,
                                          "reveal": self.reveal, "proposal": self.proposal, "actions": actions}))

    def hunks(self) -> dict[str, Hunk]:
        """The hunks of the proposed fixes, which the reader can quote like the deck's."""
        with self.lock:
            return self._fix_hunks()

    def _fix_hunks(self) -> dict[str, Hunk]:
        return {h.id: h for a in self.actions if a["kind"] == "fix" for f in fix_files(a) for h in f.hunks}

    def op(self, op: dict, by: str, ctx: OpContext) -> dict:
        kind = op.get("op") if isinstance(op.get("op"), str) else None
        if kind not in self.OPS[by]:
            raise ValueError(f"unknown operation {kind!r}")
        if "severity" in op and op["severity"] not in (None, *SEVERITIES):
            raise ValueError(f"severity: expected one of {', '.join(SEVERITIES)}")
        with self.lock:
            result = getattr(self, "_op_" + kind.replace("-", "_"))(op, by, ctx)
            self.save()
        return result

    def _ask(self, kind: str, once: bool = True, **extra):
        """Queue a request for the session, unless `once` and one of its kind already waits.
        The file is saved first, since `listen` reads it when it hands the request over."""
        if once and any(x.get("kind") == kind for x in self.queue.items()):
            return
        self.save()
        self.queue.add({"id": new_id(), "kind": kind, "at": time.time(), **extra})

    # Findings

    def _finding(self, fid) -> dict:
        item = self.get(str(fid)) if FINDING_ID.match(str(fid)) else None
        if item is None:
            raise ValueError(f"no finding {fid}")
        return item

    def _op_add(self, op: dict, by: str, ctx: OpContext) -> dict:
        text = str(op.get("text") or "").strip()
        if not text:
            raise ValueError("a finding needs text")
        refs = parse_refs(ctx.hunks, op.get("refs")) + [locate(ctx.deck, str(a)) for a in op.get("at") or []]
        slide = ctx.deck.slide(str(op.get("slide")))
        review = by == "session" and op.get("review") is True
        item = {"id": new_id("f"), "text": text[:20_000], "author": by, "severity": op.get("severity"),
                "sources": ["review" if review else "reader"], "refs": refs[:8], "slide": slide["id"] if slide else "",
                "at": time.time(), "action": None, "hidden": review and self.reveal != "shown", "merged": []}
        self.items.append(item)
        if by == "reader" and item["severity"] is None:
            self._ask("finding", once=False, finding=item)
        return {"finding": item}

    def _op_set(self, op: dict, by: str, ctx: OpContext) -> dict:
        item = self._finding(op.get("id"))
        if "text" in op:
            if by == "reader" and "reader" not in item["sources"]:
                raise ValueError("only your own findings can be reworded")
            if not str(op["text"]).strip():
                raise ValueError("a finding needs text")
        if "action" in op:
            if op["action"] not in (None, *FINDING_ACTIONS):
                raise ValueError(f"action: expected one of {', '.join(FINDING_ACTIONS)}")
            if op["action"] == "comment" and not ctx.deck.spec.get("pr"):
                raise ValueError("the deck has no pull request to comment on")
        if "text" in op:
            text = str(op["text"]).strip()[:20_000]
            if text != item["text"]:
                # Drafts that still say what the finding said follow it.
                for a in self._drafts(item["id"]):
                    if a["body"] == item["text"]:
                        a.update(body=text, author=by)
                item.update(text=text, author=by)
        if "action" in op and op["action"] != item["action"]:
            self._include(item["id"], op["action"] == "comment")
        for key in ("severity", "action"):
            if key in op:
                item[key] = op[key]
        return {"finding": item}

    def _drafts(self, fid: str) -> list[dict]:
        """The comments drafted for a finding that have not gone out yet."""
        return [a for a in self.actions if a["finding"] == fid and a["kind"] == "comment" and a["state"] in ("proposed", "failed")]

    def _include(self, fid: str, on: bool):
        """A finding's unsent comments go out only while it is marked comment."""
        for a in self._drafts(fid):
            a["include"] = on

    def _op_remove(self, op: dict, by: str, ctx: OpContext) -> dict:
        item = self._finding(op.get("id"))
        if by == "reader" and "reader" not in item["sources"]:
            raise ValueError("only your own findings can be removed")
        self.items.remove(item)
        self._include(item["id"], False)
        self._repoint(item["id"], None)
        return {"finding": item}

    def _repoint(self, old: str, new: str | None):
        """Point the actions for a finding that is gone at the one that took its place, if any."""
        for a in self.actions:
            if a["finding"] == old:
                a["finding"] = new

    def _op_merge(self, op: dict, by: str, ctx: OpContext) -> dict:
        """Fold duplicates into one finding: sources and places join, the most severe severity wins."""
        keep = self._finding(op.get("id"))
        others = [self._finding(o) for o in dict.fromkeys(op.get("others") or [])]
        if not others or keep in others:
            raise ValueError("merge needs a finding to keep and other findings to fold into it")
        for o in others:
            keep["sources"] = sorted({*keep["sources"], *o["sources"]}, key=["reader", "review"].index)
            seen = {place_key(r) for r in keep["refs"]}
            keep["refs"] += [r for r in o["refs"] if place_key(r) not in seen]
            keep["merged"] += [{"id": o["id"], "text": o["text"], "sources": o["sources"]}, *o["merged"]]
            keep["severity"] = min((x for x in (keep["severity"], o["severity"]) if x), default=None)
            keep["action"] = keep["action"] or o["action"]
            keep["hidden"] = keep["hidden"] and o["hidden"]
            self.items.remove(o)
            self._repoint(o["id"], keep["id"])
        keep["refs"] = keep["refs"][:8]
        return {"finding": keep}

    def _op_review(self, op: dict, by: str, ctx: OpContext) -> dict:
        if op.get("status") not in ("running", "done"):
            raise ValueError("status: expected running or done")
        self.review = op["status"]
        if self.review == "done" and self.reveal == "requested":
            self._ask("reveal")  # the reader asked while the review ran
        return {"review": self.review}

    def _op_request_reveal(self, op: dict, by: str, ctx: OpContext) -> dict:
        if self.reveal == "requested":
            self._ask("reveal")  # the reader asks again
        elif self.reveal == "hidden":
            if self.review == "running" or any(x["hidden"] for x in self.items):
                self.reveal = "requested"
                self._ask("reveal")
            else:
                self.reveal = "shown"
        return {"reveal": self.reveal}

    def _op_reveal(self, op: dict, by: str, ctx: OpContext) -> dict:
        for x in self.items:
            x["hidden"] = False
        self.reveal = "shown"
        return {"reveal": "shown"}

    # Proposed actions

    def _action(self, aid) -> dict:
        item = next((a for a in self.actions if a["id"] == str(aid)), None) if ACTION_ID.match(str(aid)) else None
        if item is None:
            raise ValueError(f"no action {aid}")
        return item

    def _action_fields(self, a: dict, op: dict, by: str, ctx: OpContext) -> dict:
        """The validated changes `op` makes to action `a`; nothing changes until all are valid."""
        out = {}
        if "title" in op:
            out["title"] = _required(op["title"], "title")[:200]
        if "include" in op:
            out["include"] = op["include"] is True
        if "body" in op:
            if a["kind"] != "comment":
                raise ValueError("only a comment has a body")
            out["body"] = _required(op["body"], "body")[:20_000]
            if out["body"] != a.get("body"):
                out["author"] = by
        if by == "reader" and set(op) - {"op", "id", "include", "body"}:
            raise ValueError("the reader can include, leave out or reword an action")
        if "at" in op:
            if a["kind"] != "comment":
                raise ValueError("only a comment has a place")
            out["place"] = place_of(locate(ctx.deck, str(op["at"]))) if op["at"] else None
        if "patch" in op:
            if a["kind"] != "fix":
                raise ValueError("only a fix has a patch")
            patch = _required(op["patch"], "patch")
            try:
                if not parse_diff(patch):
                    raise DiffError("it changes no files")
            except DiffError as e:
                raise ValueError(f"patch: {e}") from None
            out["patch"] = patch
        if "command" in op:
            if a["kind"] != "check":
                raise ValueError("only a check has a command")
            out["command"] = _required(op["command"], "command")
        return out

    def _op_action_add(self, op: dict, by: str, ctx: OpContext) -> dict:
        kind = op.get("kind")
        if kind not in ACTION_KINDS:
            raise ValueError(f"kind: expected one of {', '.join(ACTION_KINDS)}")
        finding = self._finding(op["finding"]) if op.get("finding") else None
        if kind == "comment" and not ctx.deck.spec.get("pr"):
            raise ValueError("the deck has no pull request to comment on")
        needed = {"comment": "body", "fix": "patch", "check": "command"}[kind]
        if kind == "comment" and not op.get("body") and finding:
            op = {**op, "body": finding["text"]}
        if not op.get(needed):
            raise ValueError(f"a {kind} needs its {needed}")
        a = {"id": new_id("a"), "kind": kind, "title": "", "finding": finding["id"] if finding else None,
             "include": True, "state": "proposed", "result": None}
        if kind == "comment":
            # By default a comment sits at its finding's first place in the diff, where GitHub can anchor it.
            inline = lambda r: r["kind"] == "lines" and r["hunk"] in ctx.deck.hunks and not ctx.deck.hunks[r["hunk"]].file.plain
            at = next((place_of(r) for r in (finding or {}).get("refs", []) if inline(r)), None)
            a["place"] = at
        a.update(self._action_fields(a, {**op, "title": op.get("title") or "untitled"}, by, ctx))
        if kind == "comment" and finding and a["body"] == finding["text"]:
            a["author"] = finding["author"]  # the finding's own words post as whoever wrote them
        if not op.get("title"):
            # A comment's title says where it goes; its finding is shown beside it.
            where = f"at {a['place']['label']}" if a.get("place") else "in the review summary"
            a["title"] = f"Comment {where}" if kind == "comment" else (finding or {}).get("text", kind)[:120]
        self.actions.append(a)
        return {"action": a}

    def _op_action_set(self, op: dict, by: str, ctx: OpContext) -> dict:
        a = self._action(op.get("id"))
        if a["state"] in ("running", "done"):
            raise ValueError(f"action {a['id']} is {a['state']}")
        a.update(self._action_fields(a, op, by, ctx))
        return {"action": a}

    def _op_action_remove(self, op: dict, by: str, ctx: OpContext) -> dict:
        a = self._action(op.get("id"))
        self.actions.remove(a)
        return {"action": a}

    def _op_action_ready(self, op: dict, by: str, ctx: OpContext) -> dict:
        self.proposal = "ready"
        return {"proposal": self.proposal}

    def _op_action_result(self, op: dict, by: str, ctx: OpContext) -> dict:
        a = self._action(op.get("id"))
        if op.get("status") not in ("done", "failed"):
            raise ValueError("status: expected done or failed")
        a["state"] = op["status"]
        a["result"] = {k: str(op[k])[:20_000] for k in ("link", "note", "output") if op.get(k)}
        return {"action": a}

    def _op_request_proposal(self, op: dict, by: str, ctx: OpContext) -> dict:
        self.proposal = "requested"
        self._ask("propose")
        return {"proposal": self.proposal}

    def _op_carry_out(self, op: dict, by: str, ctx: OpContext) -> dict:
        if self.proposal != "ready":
            raise ValueError("the session is still drafting the actions")
        ids = op.get("ids") if isinstance(op.get("ids"), list) else []
        chosen = [self._action(i) for i in dict.fromkeys(ids)]
        if not chosen:
            raise ValueError("choose the actions to carry out")
        now = time.time()
        for a in chosen:
            if a["state"] == "done":
                raise ValueError(f"action {a['id']} is done already")
            if resend_in(a, now):
                raise ValueError(f"action {a['id']} was sent to the session just now")
        for a in chosen:
            a.update(state="running", include=True, result=None, sent=now)
        self._ask("carry-out", once=False, actions=[a["id"] for a in chosen])
        return {"actions": chosen}


def resend_in(a: dict, now: float | None = None) -> float:
    """Seconds until a running action may go again, once the session has had time to report it; 0 when it may."""
    if a["state"] != "running":
        return 0
    return max(0.0, a.get("sent", 0) + CLAIM_STALE - (time.time() if now is None else now))


def _required(v, name: str) -> str:
    if not isinstance(v, str) or not v.strip():
        raise ValueError(f"{name}: expected text")
    return v.strip()


def place_of(ref: dict) -> dict:
    """Where a review comment goes: a reference's file, side and lines."""
    return {k: ref[k] for k in ("path", "side", "line", "end", "label")}


def fix_files(a: dict) -> list[FileDiff]:
    return _parse_fix(a["patch"], a["id"])


@functools.lru_cache(maxsize=64)
def _parse_fix(patch: str, label: str) -> list[FileDiff]:
    try:
        return parse_diff(patch, label)
    except DiffError:
        return []


def fix_payload(a: dict) -> dict:
    """A fix's patch as the page draws it: its files and one excerpt per file."""
    files = fix_files(a)
    blocks = [{"type": "excerpt", "file": f.key, "caption": "", "pieces": [{"hunk": h.id, "lo": 0, "hi": len(h.rows) - 1} for h in f.hunks]}
              for f in files]
    return {"files": [{**file_payload(f), "label": "proposed fix"} for f in files], "blocks": blocks}


def finding_text(f: dict) -> str:
    sev = f["severity"] or "no severity"
    where = ", ".join(r["label"] for r in f["refs"] if r.get("label"))
    hidden = ", hidden" if f.get("hidden") else ""
    return f"{f['id']} ({sev}, from {' and '.join(f['sources'])}{hidden}{', at ' + where if where else ''}): {f['text']}"


def default_diffs(deck_dir: Path) -> list:
    # Decks written before diffs became optional rely on the default.
    return ["change.diff"] if (deck_dir / "change.diff").exists() else []


def read_diffs(load: Loading, diffs) -> list:
    """Parse deck.json's `diffs` (file names or {file, label}) into load.files and load.hunks; return the list."""
    errors = load.errors
    if not isinstance(diffs, list):
        errors.append("diffs: expected a list of file names or {file, label}")
        diffs = []
    for j, d in enumerate(diffs):
        d = {"file": d} if isinstance(d, str) else d
        if not isinstance(d, dict):
            errors.append(f"diffs[{j}]: expected a file name or {{file, label}}")
            continue
        path = load.dir / str(d.get("file", ""))
        load.sources.append(path)
        try:
            load.files += parse_diff(path.read_text(errors="replace"), str(d.get("label") or ""))
        except OSError as e:
            errors.append(f"diffs[{j}]: cannot read {path}: {e.strerror}")
        except DiffError as e:
            errors.append(f"diffs[{j}]: {path.name}: {e}")
    load.hunks.update((h.id, h) for f in load.files for h in f.hunks)
    return diffs


def cmd_hunks(args):
    deck_dir = Path(args.deck)
    spec = read_json(deck_dir / "deck.json")
    load = Loading(deck_dir, spec, [], [])
    if args.diff:
        diffs = [{"file": name, "label": label} for name, _, label in (d.partition("=") for d in args.diff)]
    else:
        diffs = spec.get("diffs", default_diffs(deck_dir))
    read_diffs(load, diffs)
    if load.errors:
        sys.exit("\n".join(load.errors))
    files = load.files
    print(
        f"{len(files)} files, {sum(len(f.hunks) for f in files)} hunks, "
        f"+{sum(f.adds for f in files)} −{sum(f.dels for f in files)}\n"
    )
    for f in files:
        print(f"{f.key}  ({f.describe()}, +{f.adds} −{f.dels})")
        if not f.hunks:
            print(f"  {f.key}  file-level change: {' '.join(f.meta) or f.describe()}")
        for h in f.hunks:
            print(f"  {h.id}  {h.header}  changed {', '.join(changed_spans(h, changed(h)))}")
            if args.show:
                for i, r in enumerate(h.rows, 1):
                    o = "" if r.old is None else r.old
                    n = "" if r.new is None else r.new
                    print(f"    {i:>4} {o:>5} {n:>5} {r.kind}{r.text}")


SEVERITY_HELP = ("P0 blocks the release or major use, such as a security hole or data loss; P1 is urgent;"
                 " P2 should be fixed eventually; P3 is a nicety")


def load_review(deck_dir: Path) -> Review:
    return Review(deck_dir / "findings.json", Queue(deck_dir))


def action_text(a: dict) -> str:
    on = f" on {a['finding']}" if a["finding"] else ""
    state = a["state"] + ("" if a["include"] else ", skipped")
    return f"{a['id']} {a['kind']}{on}: {a['title']} [{state}]"


def proposal_brief(review: Review, cmd: str) -> str:
    """What the session needs to propose the review's actions."""
    words = lambda f: ", in the reader's words" if f["author"] == "reader" else ""
    findings = "\n".join(f"  {finding_text(f)} [{f['action'] or 'no choice'}{words(f)}]" for f in review.items) or "  (none)"
    actions = "\n".join(f"  {action_text(a)}" for a in review.actions) or "  (none)"
    return f"""The reader asked for proposed actions.

Findings, with the reader's choices:
{findings}

Actions proposed so far:
{actions}

Propose what the reader's choices call for, and nothing for a finding marked ignore:
- For each finding marked comment, draft one review comment. If the finding is in the reader's words, post them as written: leave out --body-file, and the comment takes the finding's text and posts as the reader. Otherwise write its body to a file, then run
    {cmd} add comment --finding ID --body-file FILE
  It goes at the finding's first place in a file; --at PATH:LINE[-END] moves it and --summary puts it in the review's summary. Write it as it will be posted: short, specific, and saying what to change.
- Propose a fix only where the reader asked for one, or where the change is the reader's own and the fix is small and certain. Write it as a patch against the head of the change, as `git diff` prints it from the repository root:
    {cmd} add fix --finding ID --title TITLE --patch-file PATCH
- Propose a check, a test or command that settles a doubt, where one would:
    {cmd} add check --title TITLE --command COMMAND
Revise a draft with `{cmd} set ID` (--body-file, --title, --at, --patch-file or --command) and drop one with `{cmd} remove ID`. Then show them to the reader, who can edit comments, ask for changes and choose what to carry out:
    {cmd} ready
Nothing runs until the reader presses Carry out."""


def carry_brief(review: Review, ids: list[str], cmd: str, pr: str) -> str:
    """What the session needs to carry out the actions the reader approved."""
    out = ["The reader approved these actions. Carry them out now:\n"]
    for a in (a for a in review.actions if a["id"] in ids):
        out.append(f"  {action_text(a)}")
        if a["kind"] == "comment":
            who = "the reader's account" if a["author"] == "reader" else "your account"
            where = f"at {a['place']['path']}:{a['place']['line']}" + (f"-{a['place']['end']}" if a["place"]["end"] != a["place"]["line"] else "") + f" ({a['place']['side']} side)" if a["place"] else "in the review's summary"
            out.append(f"    posts as {who}, {where}:\n" + "\n".join("    > " + line for line in a["body"].splitlines()))
        elif a["kind"] == "fix":
            out.append("    patch:\n" + "\n".join("    " + line for line in fenced(a["patch"], "diff").splitlines()))
        else:
            out.append(f"    command: {a['command']}")
    out.append(f"""
- Comments: post them on {pr or 'the pull request'} as review comments, one review per account. Post the comments you wrote with the GitHub identity you normally use, and the ones the reader wrote or edited with the reader's own identity; the reader's press authorizes exactly those comments under their identity, and nothing else. Submit each review as a comment: do not approve or request changes unless the reader asks. If an identity is not available to you, report those comments failed and say why; do not post them under another.
- Fixes: apply each patch with `git apply` in the repository, and leave the changes uncommitted unless the reader asks otherwise.
- Checks: run each command where the change is checked out.
Report each action as you finish it, with the comment's link or the check's output:
  {cmd} result ID --status done|failed [--link URL] [--note TEXT] [--output-file FILE]""")
    return "\n".join(out)


def reveal_brief(findings: Review, cmd: str) -> str:
    """What the session needs to merge duplicates and show the review's findings."""
    mine = [f for f in findings.items if not f.get("hidden")]
    hidden = [f for f in findings.items if f.get("hidden")]
    out = ["The reader asked to see the review's findings.\n"]
    if findings.review == "running":
        out.append("The background review is still running. Wait for it, record its findings with"
                   f" `{cmd} add --review`, and mark it done with `{cmd} review done` before revealing.\n")
    out.append("The reader's findings:\n" + ("\n".join(f"  {finding_text(f)}" for f in mine) or "  (none)"))
    out.append("\nThe review's hidden findings:\n" + ("\n".join(f"  {finding_text(f)}" for f in hidden) or "  (none)"))
    out.append(
        "\nMerge findings that report the same problem, keeping the clearest text; merging keeps both sources"
        " and the most severe severity:\n"
        f"  {cmd} merge KEEP_ID OTHER_ID...\n"
        f"Reword a merged finding with `{cmd} set ID --text TEXT` if neither text covers both. Then show them all:\n"
        f"  {cmd} reveal"
    )
    return "\n".join(out)


def settled(q: Queue, item: dict, findings: Review) -> bool:
    """Drop a request to give a finding a severity once the finding has one, or is gone."""
    if item.get("kind") != "finding":
        return False
    f = findings.get(item["finding"]["id"])
    if f is None or f["severity"]:
        q.remove(item["id"])
        return True
    return False


def send_op(deck_dir: Path, op: dict) -> dict:
    """Pass an edit to the running server and wait for its result."""
    info = server_info(deck_dir)
    if not info:
        sys.exit("No server is running for this deck; start it with `serve` first.")
    if stale_reason(info):
        sys.exit(f"The server is out of date ({stale_reason(info)}); restart it with `serve`, then retry.")
    path = Queue(deck_dir).dir / f"{new_id('op')}.json"
    write_atomic(path, json.dumps(op))
    result = path.with_suffix(".result")
    for _ in range(300):
        time.sleep(0.1)
        if result.exists():
            data = read_json(result)
            result.unlink()
            if "error" in data:
                sys.exit(data["error"])
            return data
    if not path.exists():
        sys.exit("the server took the edit but has not answered; check with `list` before trying again")
    path.unlink(missing_ok=True)
    sys.exit("the server did not answer; see server.log")


def cmd_finding(args):
    deck_dir = Path(args.deck).resolve()
    if args.action == "list":
        findings = load_review(deck_dir)
        print(f"review: {findings.review}, findings {findings.reveal}")
        for f in findings.items:
            print(f"{finding_text(f)} [{f['action'] or 'no choice'}]")
        return
    op = {"op": args.action}
    if args.action == "add":
        op.update(text=args.text, at=args.at or [], slide=args.slide, review=args.review)
    elif args.action == "review":
        op["status"] = args.status
    elif args.action != "reveal":
        op["id"] = args.id
    if args.action == "merge":
        op["others"] = args.others
    if getattr(args, "severity", None):
        op["severity"] = None if args.severity == "none" else args.severity
    if args.action == "set" and args.text:
        op["text"] = args.text
    result = send_op(deck_dir, op)
    if "finding" in result:
        print(f"{'Removed' if args.action == 'remove' else 'Saved'} {finding_text(result['finding'])}")
    else:
        print(f"Saved: {json.dumps(result)}")


def cmd_action(args):
    deck_dir = Path(args.deck).resolve()
    if args.action == "list":
        review = load_review(deck_dir)
        print(f"proposal: {review.proposal}")
        for a in review.actions:
            print(action_text(a))
        return
    op = {"op": "action-" + args.action}
    if args.action in ("set", "remove", "result"):
        op["id"] = args.id
    if args.action == "add":
        op.update(kind=args.kind, finding=args.finding)
    if args.action in ("add", "set"):
        for key, value in (("title", args.title), ("command", args.command), ("at", args.at)):
            if value:
                op[key] = value
        if args.summary:
            op["at"] = ""
        for key, path in (("body", args.body_file), ("patch", args.patch_file)):
            if path:
                op[key] = Path(path).read_text()
    if args.action == "result":
        op.update(status=args.status, link=args.link, note=args.note,
                  output=Path(args.output_file).read_text(errors="replace")[-20_000:] if args.output_file else None)
    result = send_op(deck_dir, op)
    if "action" in result:
        print(f"{'Removed' if args.action == 'remove' else 'Saved'} {action_text(result['action'])}")
    else:
        print(f"Saved: {json.dumps(result)}")


# --------------------------------------------------------------------------
# The extension


class ReviewExtension(Extension):
    name = "review"
    assets = Path(__file__).resolve().parent.parent / "assets"
    blocks = ("diff",)
    spec_keys = ("diffs", "appendix", "pr")
    quote_sources = {"finding": "a finding", "action": "a proposed action"}

    def __init__(self):
        self._seen: tuple = (None, None)  # (findings.json and its mtime, the Review read from it)

    def begin(self, load: Loading):
        """Parse the deck's diffs; a deck with diffs needs no goal."""
        spec, errors = load.spec, load.errors
        diffs = read_diffs(load, spec.get("diffs", default_diffs(load.dir)))
        if diffs and not load.files and not errors:
            errors.append("the diffs contain no changes")
        if _str(spec.get("pr"), "pr", errors) and not PR_URL.match(spec["pr"]):
            errors.append("pr: expected a pull request URL, https://github.com/OWNER/REPO/pull/N")
        load.goal_optional = bool(diffs)
        load.extra["review"] = Shown(bool(diffs))

    def block(self, load: Loading, key: str, blk: dict, where: str, out: list[dict]):
        diff_excerpt(load, blk, where, out)

    def reserves(self, slide_id: str) -> bool:
        return slide_id.startswith("appendix-") or slide_id in (FINDINGS_SLIDE["id"], ACTIONS_SLIDE["id"])

    def end(self, load: Loading) -> list[dict]:
        """Lay out the appendix, check that every changed line is shown, and add the review's slides."""
        shown: Shown = load.extra["review"]
        errors = load.errors
        groups = []
        appendix = load.spec.get("appendix") or []
        if not isinstance(appendix, list):
            errors.append("appendix: expected a list of {title, note, diffs}")
            appendix = []
        for g, grp in enumerate(appendix):
            gw = f"appendix[{g}]"
            if not isinstance(grp, dict) or not isinstance(grp.get("diffs"), list) or not grp["diffs"]:
                errors.append(f"{gw}: expected {{title, note, diffs: [{{diff, range}}, ...]}}")
                continue
            out = {
                "title": _str(grp.get("title"), f"{gw}.title", errors, True),
                "note": _str(grp.get("note"), f"{gw}.note", errors, True),
                "blocks": [],
            }
            for b, blk in enumerate(grp["diffs"]):
                bw = f"{gw}.diffs[{b}]"
                if not isinstance(blk, dict) or "diff" not in blk:
                    errors.append(f"{bw}: expected {{diff, range, caption}}")
                    continue
                diff_excerpt(load, blk, bw, out["blocks"])
            groups.append(out)
        for f in load.files:
            if not f.hunks and f.key not in shown.files:
                errors.append(f"{f.key} ({f.describe()}) is not shown; add {json.dumps({'diff': f.key})}")
            for h in f.hunks:
                gaps = _uncovered(h, shown.rows[h.id])
                if gaps and gaps == [(min(needed(h)), max(needed(h)))] and not shown.rows[h.id]:
                    errors.append(f"{h.id} ({row_span(h.rows, 0, len(h.rows) - 1)}) is not shown; "
                                  f"add {json.dumps({'diff': h.id})}")
                    continue
                for lo, hi in gaps:
                    ref = {"diff": h.id, "range": f"rows:{lo + 1}-{hi + 1}"}
                    errors.append(f"{h.id}: changed lines {row_span(h.rows, lo, hi)} are not shown; "
                                  f"add {json.dumps(ref)}")
        del load.extra["review"]
        if not shown.diffs:
            return _appendix_slides(groups) if groups else []
        diff_hunks = [h for f in load.files for h in f.hunks]
        in_appendix = {(p["hunk"], i) for g in groups for b in g["blocks"] for p in b["pieces"]
                       for i in range(p["lo"], p["hi"] + 1)}
        load.extra["coverage"] = {
            "changed": sum(len(needed(h)) for h in diff_hunks),
            "appendix": sum((h.id, i) in in_appendix for h in diff_hunks for i in needed(h)),
            "files": len(load.files),
        }
        generated = _appendix_slides(groups) if groups else []
        return generated + json.loads(json.dumps([FINDINGS_SLIDE, ACTIONS_SLIDE]))

    def payload(self, deck: Deck) -> dict:
        diffs = [f for f in deck.files if not f.plain]
        return {
            "pr": deck.spec.get("pr", ""),
            "stats": {"files": len(diffs), "adds": sum(f.adds for f in diffs), "dels": sum(f.dels for f in diffs)},
            "coverage": deck.extra.get("coverage"),
        }

    def report(self, deck: Deck) -> str:
        c = deck.extra.get("coverage")
        if not c:
            return ""
        return (f"; all {c['changed']} non-blank changed lines in {c['files']} files are shown "
                f"({c['appendix']} only in the appendix)")

    def attach(self, state):
        state.review = Review(state.deck_dir / "findings.json", state.queue)

    def tick(self, state):
        """Apply the session's edits, which `finding` and `action` leave in queue/op-*.json."""
        for path in state.queue.dir.glob("op-*.result"):
            with contextlib.suppress(OSError):
                if time.time() - path.stat().st_mtime > 300:
                    path.unlink()  # nobody waited for it
        for path in sorted(state.queue.dir.glob("op-*.json")):
            op = read_json(path)
            try:
                result = review_op(state, op, "session")
            except ValueError as e:
                result = {"error": str(e)}
            except Exception as e:  # a bug here must not stop the watcher
                log(f"{path.name}: {e!r}")
                result = {"error": f"the server failed: {e!r}; see server.log"}
            write_atomic(path.with_suffix(".result"), json.dumps(result))
            path.unlink(missing_ok=True)

    def hunks(self, state) -> dict[str, Hunk]:
        return state.review.hunks()

    def get(self, state, route: str):
        return state.review.snapshot() if route == "/api/review" else None

    def post(self, state, route: str, body: dict):
        return review_op(state, body, "reader") if route == "/api/review" else None

    def skip(self, q: Queue, item: dict) -> bool:
        return item.get("kind") == "finding" and settled(q, item, self._review_now(q.dir.parent))

    def _review_now(self, deck_dir: Path) -> Review:
        """The findings as saved, read again only when findings.json changes."""
        path = deck_dir / "findings.json"
        stamp = path.stat().st_mtime_ns if path.exists() else 0
        if self._seen[0] != (path, stamp):
            self._seen = ((path, stamp), load_review(deck_dir))
        return self._seen[1]

    def listen(self, item: dict, ctx: Listening) -> bool:
        kind, me, deck_arg = item.get("kind"), ctx.me, ctx.deck_arg
        if kind == "finding":
            f = item["finding"]
            print(f"The reader added a finding without a severity:\n  {finding_text(f)}\n\n"
                  f"Give it one ({SEVERITY_HELP}):\n  python3 {me} finding {deck_arg} set {f['id']} --severity P2")
        elif kind in ("reveal", "propose", "carry-out"):
            review = load_review(ctx.deck_dir)
            if kind == "reveal":
                print(reveal_brief(review, f"python3 {me} finding {deck_arg}"))
            elif kind == "propose":
                print(proposal_brief(review, f"python3 {me} action {deck_arg}"))
            else:
                pr = read_json(ctx.deck_dir / "deck.json").get("pr", "")
                print(carry_brief(review, item["actions"], f"python3 {me} action {deck_arg}", pr))
        else:
            return False
        ctx.q.remove(item["id"])
        print(f"Then keep listening with:\n  {ctx.again}")
        return True

    def finished(self, ctx: Listening) -> str:
        review = load_review(ctx.deck_dir)
        findings = review.items
        out = []
        if findings:
            out.append("\nFindings:\n" + "\n".join(f"  {finding_text(f)} [{f['action'] or 'no choice'}]" for f in findings))
        if any(f["hidden"] for f in findings) or review.review == "running":
            cmd = f"python3 {ctx.me} finding {ctx.deck_arg}"
            wait = (f" The background review is still running: wait for it, record its findings and run `{cmd} review done`."
                    if review.review == "running" else "")
            out.append(f"\nThe reader has not seen the review's findings.{wait} Merge duplicates (`{cmd} merge`),"
                       f" run `{cmd} reveal`, and go over them in the wrap-up.")
        return "\n".join(out)

    def commands(self, sub):
        h = sub.add_parser("hunks", help="list hunk ids in the deck's diffs")
        h.add_argument("deck", help="deck directory")
        h.add_argument(
            "--diff",
            action="append",
            metavar="FILE[=LABEL]",
            help="diff file in the deck directory (default: the deck's diffs, or change.diff)",
        )
        h.add_argument("--show", action="store_true", help="print every row with its row number")
        h.set_defaults(func=cmd_hunks)

        f = sub.add_parser("finding", help="add, change, remove or list review findings")
        f.add_argument("deck")
        fsub = f.add_subparsers(dest="action", required=True)
        fa = fsub.add_parser("add", help="add a finding for the reader")
        fa.add_argument("text")
        fa.add_argument("--severity", choices=SEVERITIES)
        fa.add_argument("--at", action="append", metavar="PATH:LINE[-END]", help="where, by new-side line numbers")
        fa.add_argument("--slide", help="the slide it came up on")
        fa.add_argument("--review", action="store_true", help="a finding of the background review, hidden until the reader reveals them")
        fs = fsub.add_parser("set", help="change a finding's severity or text")
        fs.add_argument("id")
        fs.add_argument("--severity", choices=[*SEVERITIES, "none"])
        fs.add_argument("--text")
        fr = fsub.add_parser("remove", help="remove a finding")
        fr.add_argument("id")
        fm = fsub.add_parser("merge", help="fold duplicate findings into one")
        fm.add_argument("id", help="the finding to keep")
        fm.add_argument("others", nargs="+", help="the findings to fold into it")
        fv = fsub.add_parser("review", help="record the background review's status")
        fv.add_argument("status", choices=["running", "done"])
        fsub.add_parser("reveal", help="show the review's findings to the reader")
        fsub.add_parser("list", help="print the findings, hidden ones included")
        f.set_defaults(func=cmd_finding)

        a = sub.add_parser("action", help="propose, revise and report the review's actions")
        a.add_argument("deck")
        asub = a.add_subparsers(dest="action", required=True)
        aa = asub.add_parser("add", help="propose a comment, fix or check")
        aa.add_argument("kind", choices=ACTION_KINDS)
        aa.add_argument("--finding", help="the finding it acts on")
        asets = [aa, asub.add_parser("set", help="revise a proposed action")]
        asets[1].add_argument("id")
        for ap in asets:
            ap.add_argument("--title")
            ap.add_argument("--body-file", help="a comment's text, Markdown (on add, default: its finding's text)")
            ap.add_argument("--at", metavar="PATH:LINE[-END]", help="where a comment goes (on add, default: its finding's first place)")
            ap.add_argument("--summary", action="store_true", help="put a comment in the review's summary")
            ap.add_argument("--patch-file", help="a fix, as a unified diff from the repository root")
            ap.add_argument("--command", help="a check's command")
        asub.add_parser("remove", help="drop a proposed action").add_argument("id")
        asub.add_parser("ready", help="show the proposed actions to the reader")
        ar = asub.add_parser("result", help="report how an action went")
        ar.add_argument("id")
        ar.add_argument("--status", choices=["done", "failed"], required=True)
        ar.add_argument("--link", help="the posted comment's URL")
        ar.add_argument("--note", help="one line for the reader")
        ar.add_argument("--output-file", help="a check's output")
        asub.add_parser("list", help="print the proposed actions")
        a.set_defaults(func=cmd_action)


class Shown:
    """The changed lines and files a deck's diff excerpts show, while it loads."""

    def __init__(self, diffs: bool):
        self.diffs = diffs
        self.rows: dict[str, set[int]] = defaultdict(set)
        self.files: set[str] = set()


def diff_excerpt(load: Loading, blk: dict, where: str, blocks: list[dict]):
    """Add a diff excerpt to `blocks` and record the lines it shows."""
    files = {f.key: f for f in load.files}
    ex = _excerpt(blk, where, files, load.hunks, load.errors)
    if not ex:
        return
    shown: Shown = load.extra["review"]
    for p in ex["pieces"]:
        shown.rows[p["hunk"]].update(range(p["lo"], p["hi"] + 1))
    if not ex["pieces"]:
        shown.files.add(ex["file"])
    blocks.append({"type": "excerpt", **ex, "caption": _str(blk.get("caption"), f"{where}.caption", load.errors)})


def review_op(state, op: dict, by: str) -> dict:
    """Apply an edit to the findings or proposed actions and tell the pages."""
    result = state.review.op(op, by, OpContext(state.deck, state.hunks()))
    state.broadcast("review", {})
    return result


if not any(e.name == ReviewExtension.name for e in core.EXTENSIONS):
    core.EXTENSIONS.append(ReviewExtension())

if __name__ == "__main__":
    core.main(entry=__file__, description=__doc__)
