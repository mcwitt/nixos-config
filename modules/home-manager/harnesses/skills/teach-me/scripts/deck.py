#!/usr/bin/env python3
"""Check and serve a teaching slideshow.

`check` validates a deck, reports which slides its checks cover and, for a
deck with diffs, which changed lines each slide shows. `hunks` lists the hunks
of a deck's diffs with the ids slides refer to. `serve` publishes the deck as
a local web page with a chat panel on every slide. `listen` hands the reader's
questions to the session and posts its answers back; `stop` ends a detached
server.

A deck directory holds `deck.json`, any unified diffs it names and any text it
saved for excerpts. The server
writes `threads.json` (the chat history), `server.json`, `server.log` and a
`queue/` directory next to them. Questions and answers cross between the
server and `listen` only as files in `queue/`, so `listen` needs no network
access. Requires Python >= 3.11; uses only the standard library.
"""

from __future__ import annotations

import argparse
import codecs
import json
import os
import queue
import re
import secrets
import signal
import socket
import subprocess
import sys
import threading
import time
from collections import defaultdict
from dataclasses import dataclass, field
from http.server import BaseHTTPRequestHandler, ThreadingHTTPServer
from pathlib import Path
from urllib.parse import urlsplit

ASSETS = Path(__file__).resolve().parent.parent / "assets"
SCRIPT = Path(__file__).resolve()
SLIDE_ID = re.compile(r"^[A-Za-z][A-Za-z0-9._~-]{0,63}$")
QUESTION_ID = re.compile(r"^q-[0-9a-z-]+$")
QUOTE_SOURCES = {"diagram": "a diagram", "callout": "a callout", "text": "the slide's text", "quiz": "a check", "math": "a formula"}
HUNK_RE = re.compile(r"^@@ -(\d+)(?:,(\d+))? \+(\d+)(?:,(\d+))? @@ ?(.*)$")
RANGE_RE = re.compile(r"^(new|old|rows):(\d+)(?:-(\d+))?$")
LINES_RE = re.compile(r"^(\d+)(?:-(\d+))?$")
HEARTBEAT = 15.0
# The session re-runs `listen` after each timeout; treat it as listening for
# this long after the last run so the page does not flicker between runs.
LISTEN_GRACE = 90.0
CLAIM_STALE = 20 * 60

LANGUAGES = {
    ".nix": "nix", ".py": "python", ".pyi": "python", ".js": "javascript",
    ".mjs": "javascript", ".cjs": "javascript", ".jsx": "javascript",
    ".ts": "typescript", ".tsx": "typescript", ".rs": "rust", ".go": "go",
    ".c": "c", ".h": "c", ".cc": "cpp", ".cpp": "cpp", ".cxx": "cpp",
    ".hpp": "cpp", ".cu": "cpp", ".cuh": "cpp", ".java": "java",
    ".kt": "kotlin", ".swift": "swift", ".rb": "ruby", ".lua": "lua",
    ".sh": "bash", ".bash": "bash", ".zsh": "bash", ".json": "json",
    ".yaml": "yaml", ".yml": "yaml", ".toml": "ini", ".ini": "ini",
    ".md": "markdown", ".css": "css", ".scss": "scss", ".html": "xml",
    ".xml": "xml", ".svg": "xml", ".sql": "sql", ".hs": "haskell",
    ".el": "lisp", ".php": "php", ".r": "r", ".cs": "csharp",
}
LANGUAGE_NAMES = {"Makefile": "makefile", "Dockerfile": "dockerfile", "flake.lock": "json"}


# --------------------------------------------------------------------------
# Unified diffs


@dataclass
class Row:
    kind: str  # " ", "+" or "-"
    old: int | None
    new: int | None
    text: str
    noeol: bool = False


@dataclass
class Hunk:
    id: str
    file: "FileDiff"
    header: str
    heading: str
    old_start: int
    old_count: int
    new_start: int
    new_count: int
    rows: list[Row] = field(default_factory=list)

    def changed(self) -> set[int]:
        return {i for i, r in enumerate(self.rows) if r.kind != " "}

    def needed(self) -> set[int]:
        """Changed rows that coverage requires: blank lines carry nothing to show."""
        return {i for i in self.changed() if self.rows[i].text.strip()}


@dataclass
class FileDiff:
    key: str
    label: str
    old_path: str | None
    new_path: str | None
    status: str = "modified"
    binary: bool = False
    plain: bool = False  # a file quoted as is, not a diff
    meta: list[str] = field(default_factory=list)
    hunks: list[Hunk] = field(default_factory=list)

    @property
    def path(self) -> str:
        return self.new_path or self.old_path or "?"

    @property
    def adds(self) -> int:
        return sum(r.kind == "+" for h in self.hunks for r in h.rows)

    @property
    def dels(self) -> int:
        return sum(r.kind == "-" for h in self.hunks for r in h.rows)

    @property
    def language(self) -> str:
        name = Path(self.path).name
        return LANGUAGE_NAMES.get(name) or LANGUAGES.get(Path(name).suffix.lower(), "")

    def describe(self) -> str:
        parts = [self.status]
        if self.status in ("renamed", "copied"):
            parts.append(f"from {self.old_path}")
        if self.binary:
            parts.append("binary")
        return ", ".join(parts)


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


def row_span(rows: list[Row], lo: int, hi: int) -> str:
    """Describe rows lo..hi by line numbers, e.g. `new 12–18` or `old 4`."""
    news = [r.new for r in rows[lo : hi + 1] if r.new is not None]
    olds = [r.old for r in rows[lo : hi + 1] if r.old is not None]
    side, nums = ("new", news) if news else ("old", olds)
    if not nums:
        return "?"
    return f"{side} {nums[0]}" if nums[0] == nums[-1] else f"{side} {nums[0]}–{nums[-1]}"


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


def span(h: Hunk, lo: int, hi: int) -> str:
    """Rows lo..hi as a reader sees them: `new 12–18` in a diff, `12–18` in a plain excerpt."""
    s = row_span(h.rows, lo, hi)
    return s.removeprefix("new ") if h.file.plain else s


def changed_spans(h: Hunk, indices: set[int]) -> list[str]:
    runs: list[tuple[int, int]] = []
    for i in sorted(indices):
        if runs and runs[-1][1] == i - 1:
            runs[-1] = (runs[-1][0], i)
        else:
            runs.append((i, i))
    return [row_span(h.rows, lo, hi) for lo, hi in runs]


def numbered(h: Hunk, lo: int = 0, hi: int | None = None) -> str:
    hi = len(h.rows) - 1 if hi is None else hi
    if h.file.plain:
        return "\n".join(f"{r.new:>5}  {r.text}" for r in h.rows[lo : hi + 1])
    out = [f"{h.file.path}  {h.header}"]
    for r in h.rows[lo : hi + 1]:
        o = "" if r.old is None else r.old
        n = "" if r.new is None else r.new
        out.append(f"{o:>5} {n:>5} {r.kind}{r.text}")
        if r.noeol:
            out.append("            \\ No newline at end of file")
    return "\n".join(out)


# --------------------------------------------------------------------------
# Decks
#
# Slides are free-form. A deck with diffs must show every changed line on some
# slide or appendix group. A deck past TRIVIAL content slides must end with a
# check, and a deck without diffs must state its goal.

APPENDIX_ROWS = 160  # rows per appendix slide before groups spill onto the next
UNCOVERED_GAP = 6  # uncovered runs closer than this are reported together
QUIZ_QUESTIONS = (1, 6)  # questions per check slide
QUIZ_CHOICES = (2, 5)  # choices per question
TRIVIAL = 3  # content slides a deck may have without a final check


class DeckInvalid(Exception):
    def __init__(self, errors: list[str]):
        super().__init__("\n".join(errors))
        self.errors = errors


@dataclass
class Deck:
    dir: Path
    spec: dict
    files: list[FileDiff]  # diffs, then plain excerpt sources
    hunks: dict[str, Hunk]
    slides: list[dict]  # authored slides, then the generated appendix
    coverage: dict | None  # None without diffs
    sources: list[Path]

    def slide(self, slide_id: str) -> dict | None:
        return next((s for s in self.slides if s["id"] == slide_id), None)

    def position(self, s: dict) -> str:
        where = f"slide {self.slides.index(s) + 1} of {len(self.slides)}"
        return f"{where}, {s['section']}" if s["section"] else where

    def checks(self) -> list[dict]:
        return [s for s in self.slides if s["quiz"]]

    def main(self) -> list[dict]:
        """The authored slides outside the appendix."""
        return [s for s in self.slides if s["section"] != "Appendix"]

    def unchecked(self) -> list[dict]:
        """Content slides outside the appendix that no check question points at."""
        seen = {x for c in self.checks() for q in c["quiz"] for x in (q["see"], *(ch["see"] for ch in q["choices"]))}
        return [s for s in self.main() if not s["quiz"] and s["id"] not in seen]


def _str(v, where: str, errors: list[str], required: bool = False) -> str:
    if v is None and not required:
        return ""
    if not isinstance(v, str) or (required and not v.strip()):
        errors.append(f"{where}: expected {'a non-empty ' if required else 'a '}string")
        return ""
    return v


def _see(v, where: str, sid: str, targets: set[str], errors: list[str], required: bool = False) -> str:
    if v is None and not required:
        return ""
    if not isinstance(v, str) or v not in targets or v == sid:
        errors.append(f"{where}: expected the id of a content slide that settles the question")
        return ""
    return v


def _quiz(quiz, where: str, sid: str, targets: set[str], errors: list[str]) -> list[dict]:
    """Normalize a check slide's questions; `targets` are the content slide ids."""
    lo, hi = QUIZ_QUESTIONS
    if not isinstance(quiz, list) or not lo <= len(quiz) <= hi:
        errors.append(f"{where}.quiz: expected a list of {lo} to {hi} questions")
        return []
    out = []
    for i, q in enumerate(quiz):
        qw = f"{where}.quiz[{i}]"
        if not isinstance(q, dict):
            errors.append(f"{qw}: expected {{q, see, choices}}")
            continue
        item = {
            "q": _str(q.get("q"), f"{qw}.q", errors, True),
            "see": _see(q.get("see"), f"{qw}.see", sid, targets, errors, True),
            "choices": [],
        }
        choices = q.get("choices")
        lo, hi = QUIZ_CHOICES
        if not isinstance(choices, list) or not lo <= len(choices) <= hi:
            errors.append(f"{qw}.choices: expected {lo} to {hi} choices")
            choices = []
        for j, c in enumerate(choices):
            cw = f"{qw}.choices[{j}]"
            if not isinstance(c, dict):
                errors.append(f"{cw}: expected {{text, why, correct, see}}")
                continue
            item["choices"].append(
                {
                    "text": _str(c.get("text"), f"{cw}.text", errors, True),
                    "why": _str(c.get("why"), f"{cw}.why", errors, True),
                    "correct": c.get("correct", False) is True,
                    "see": _see(c.get("see"), f"{cw}.see", sid, targets, errors),
                }
            )
        n = sum(c["correct"] for c in item["choices"])
        if choices and n != 1:
            errors.append(f'{qw}.choices: expected exactly one choice with "correct": true, found {n}')
        out.append(item)
    return out


def _diagram(d, where: str, errors: list[str]) -> dict | None:
    if not isinstance(d, dict):
        errors.append(f"{where}: diagram must be an object")
        return None
    kind = d.get("type")
    out = {"type": kind, "caption": _str(d.get("caption"), f"{where}.caption", errors)}
    if kind == "svg":
        out["svg"] = _str(d.get("svg"), f"{where}.svg", errors, True)
    elif kind == "mermaid":
        out["source"] = _str(d.get("source"), f"{where}.source", errors, True)
    elif kind == "steps":
        frames = d.get("frames")
        if not isinstance(frames, list) or len(frames) < 2:
            errors.append(f"{where}.frames: expected at least two frames")
            frames = []
        out["frames"] = []
        for j, fr in enumerate(frames):
            fw = f"{where}.frames[{j}]"
            if not isinstance(fr, dict) or ("svg" in fr) == ("mermaid" in fr):
                errors.append(f"{fw}: expected an object with exactly one of svg or mermaid")
                continue
            out["frames"].append(
                {
                    "svg": _str(fr.get("svg"), f"{fw}.svg", errors) or None,
                    "mermaid": _str(fr.get("mermaid"), f"{fw}.mermaid", errors) or None,
                    "caption": _str(fr.get("caption"), f"{fw}.caption", errors),
                }
            )
        autoplay = d.get("autoplay", 0)
        if not isinstance(autoplay, (int, float)) or autoplay < 0:
            errors.append(f"{where}.autoplay: expected milliseconds per frame")
            autoplay = 0
        out["autoplay"] = autoplay
    elif kind == "html":
        out["html"] = _str(d.get("html"), f"{where}.html", errors, True)
        out["script"] = _str(d.get("script"), f"{where}.script", errors)
    else:
        errors.append(f"{where}.type: expected svg, mermaid, steps or html")
        return None
    return out


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


def _plain_excerpt(blk: dict, where: str, root: Path, files: dict[str, FileDiff], hunks: dict[str, Hunk],
                   sources: list[Path], errors: list[str]) -> dict | None:
    """Resolve {"excerpt": path, "range": "A-B"} to a hunk of unchanged lines."""
    name, spec = blk["excerpt"], blk.get("range")
    if not isinstance(name, str) or not name.strip():
        errors.append(f"{where}.excerpt: expected a file path")
        return None
    path = root / name
    if path not in sources:
        sources.append(path)
    try:
        # Split on newlines only: splitlines() also breaks at form feeds and
        # other separators, which would shift line numbers off the file's.
        lines = path.read_text(errors="replace").removesuffix("\n").split("\n")
        lines = [s.removesuffix("\r") for s in lines] if lines != [""] else []
    except OSError as e:
        errors.append(f"{where}: cannot read {path}: {e.strerror}")
        return None
    m = LINES_RE.match(str(spec or f"1-{len(lines)}").replace(" ", ""))
    a, b = (int(m[1]), int(m[2] or m[1])) if m else (0, 0)
    if not m or not 1 <= a <= b <= len(lines):
        errors.append(f"{where}.range: expected A-B within lines 1-{len(lines)} of {name}")
        return None
    f = files.get(name)
    if f is None:
        # A distinct key, so a file can appear both in a diff and as is.
        f = files[name] = FileDiff(key=f"plain:{name}", label="", old_path=None, new_path=name, status="source", plain=True)
    hid = f"{name}#L{a}-{b}"
    if hid not in hunks:
        h = Hunk(hid, f, "", "", 0, 0, a, b - a + 1, [Row(" ", None, n, lines[n - 1]) for n in range(a, b + 1)])
        f.hunks.append(h)
        hunks[hid] = h
    return {"file": f.key, "pieces": [{"hunk": hid, "lo": 0, "hi": b - a}]}


def _uncovered(h: Hunk, shown: set[int]) -> list[tuple[int, int]]:
    runs: list[list[int]] = []
    for i in sorted(h.needed() - shown):
        if runs and i - runs[-1][1] <= UNCOVERED_GAP:
            runs[-1][1] = i
        else:
            runs.append([i, i])
    return [(lo, hi) for lo, hi in runs]


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
        blocks = []
        for g in page:
            heading = "" if len(page) == 1 else f"### {g['title']}\n\n"
            blocks.append({"type": "md", "md": heading + g["note"]})
            blocks += g["blocks"]
        out.append({"id": f"appendix-{n}", "title": title, "section": "Appendix",
                    "prompts": [], "blocks": blocks, "quiz": []})
    return out


def load_deck(deck_dir: Path) -> Deck:
    deck_dir = deck_dir.resolve()
    spec_path = deck_dir / "deck.json"
    try:
        spec = json.loads(spec_path.read_text())
    except FileNotFoundError:
        raise DeckInvalid([f"{spec_path}: not found"])
    except json.JSONDecodeError as e:
        raise DeckInvalid([f"{spec_path}: invalid JSON: {e}"])
    if not isinstance(spec, dict):
        raise DeckInvalid([f"{spec_path}: expected a JSON object"])
    errors: list[str] = []

    sources = [spec_path]
    files: list[FileDiff] = []
    # Decks written before diffs became optional rely on the default.
    diffs = spec.get("diffs", ["change.diff"] if (deck_dir / "change.diff").exists() else [])
    if not isinstance(diffs, list):
        errors.append("diffs: expected a list of file names or {file, label}")
        diffs = []
    for j, d in enumerate(diffs):
        d = {"file": d} if isinstance(d, str) else d
        if not isinstance(d, dict):
            errors.append(f"diffs[{j}]: expected a file name or {{file, label}}")
            continue
        path = deck_dir / str(d.get("file", ""))
        sources.append(path)
        try:
            files += parse_diff(path.read_text(errors="replace"), str(d.get("label") or ""))
        except OSError as e:
            errors.append(f"diffs[{j}]: cannot read {path}: {e.strerror}")
        except DiffError as e:
            errors.append(f"diffs[{j}]: {path.name}: {e}")
    if diffs and not files and not errors:
        errors.append("the diffs contain no changes")
    by_key = {f.key: f for f in files}
    hunks = {h.id: h for f in files for h in f.hunks}
    plain: dict[str, FileDiff] = {}
    root = deck_dir / _str(spec.get("root"), "root", errors)

    _str(spec.get("title"), "title", errors, True)
    for key in ("subtitle", "scope", "source_url"):
        _str(spec.get(key), key, errors)
    goal = spec.get("goal")
    if not diffs or goal is not None:
        _str(goal, "goal", errors, True)
    slides_in = spec.get("slides")
    if not isinstance(slides_in, list) or not slides_in:
        errors.append("slides: expected a non-empty list")
        slides_in = []

    shown: dict[str, set[int]] = defaultdict(set)
    shown_files: set[str] = set()
    seen_ids: set[str] = set()
    slides: list[dict] = []
    # Checks point at the content slides that settle their questions.
    content_ids = {s.get("id") for s in slides_in if isinstance(s, dict) and "quiz" not in s}
    for k, s in enumerate(slides_in):
        where = f"slides[{k}]"
        if not isinstance(s, dict):
            errors.append(f"{where}: expected an object")
            continue
        sid = s.get("id")
        if not isinstance(sid, str) or not SLIDE_ID.match(sid):
            errors.append(f"{where}.id: expected letters, digits and . _ ~ - (starting with a letter)")
            sid = f"slide-{k}"
        elif sid in seen_ids:
            errors.append(f"{where}.id: duplicate id {sid!r}")
        elif sid.startswith("appendix-"):
            errors.append(f"{where}.id: ids starting with appendix- are reserved for the generated appendix")
        seen_ids.add(sid)
        where = f"slide {sid!r}"
        out = {
            "id": sid,
            "title": _str(s.get("title"), f"{where}.title", errors, True),
            "section": _str(s.get("section"), f"{where}.section", errors),
            "prompts": [],
            "blocks": [],
            "quiz": [],
        }
        prompts = s.get("prompts") or []
        if not isinstance(prompts, list) or not all(isinstance(p, str) for p in prompts):
            errors.append(f"{where}.prompts: expected a list of strings")
        else:
            out["prompts"] = prompts
        if "quiz" in s:
            if "blocks" in s:
                errors.append(f"{where}: a check slide has quiz, not blocks")
            out["quiz"] = _quiz(s["quiz"], where, sid, content_ids, errors)
            slides.append(out)
            continue
        blocks = s.get("blocks")
        if not isinstance(blocks, list) or not blocks:
            errors.append(f"{where}.blocks: expected a non-empty list")
            blocks = []
        for b, blk in enumerate(blocks):
            bw = f"{where}.blocks[{b}]"
            keys = {"md", "callout", "diff", "excerpt", "diagram"} & set(blk if isinstance(blk, dict) else ())
            if len(keys) != 1:
                errors.append(f"{bw}: expected exactly one of md, callout, diff, excerpt or diagram")
                continue
            (key,) = keys
            if key == "md":
                out["blocks"].append({"type": "md", "md": _str(blk["md"], f"{bw}.md", errors, True)})
            elif key == "callout":
                tone = blk.get("tone", "note")
                if tone not in ("note", "risk"):
                    errors.append(f"{bw}.tone: expected note or risk")
                md = _str(blk["callout"], f"{bw}.callout", errors, True)
                out["blocks"].append({"type": "callout", "tone": tone, "md": md})
            elif key == "diagram":
                d = _diagram(blk["diagram"], f"{bw}.diagram", errors)
                if d:
                    out["blocks"].append({"type": "diagram", "diagram": d})
            elif key == "excerpt":
                ex = _plain_excerpt(blk, bw, root, plain, hunks, sources, errors)
                if ex:
                    out["blocks"].append({"type": "excerpt", **ex, "caption": _str(blk.get("caption"), f"{bw}.caption", errors)})
            else:
                ex = _excerpt(blk, bw, by_key, hunks, errors)
                if ex:
                    ex = {"type": "excerpt", **ex, "caption": _str(blk.get("caption"), f"{bw}.caption", errors)}
                    for p in ex["pieces"]:
                        shown[p["hunk"]].update(range(p["lo"], p["hi"] + 1))
                    if not ex["pieces"]:
                        shown_files.add(ex["file"])
                    out["blocks"].append(ex)
        slides.append(out)

    groups = []
    appendix = spec.get("appendix") or []
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
            ex = _excerpt(blk, bw, by_key, hunks, errors)
            if ex:
                ex = {"type": "excerpt", **ex, "caption": _str(blk.get("caption"), f"{bw}.caption", errors)}
                for p in ex["pieces"]:
                    shown[p["hunk"]].update(range(p["lo"], p["hi"] + 1))
                if not ex["pieces"]:
                    shown_files.add(ex["file"])
                out["blocks"].append(ex)
        groups.append(out)

    for f in files:
        if not f.hunks and f.key not in shown_files:
            errors.append(f"{f.key} ({f.describe()}) is not shown; add {json.dumps({'diff': f.key})}")
        for h in f.hunks:
            runs = _uncovered(h, shown[h.id])
            if runs and runs == [(min(h.needed()), max(h.needed()))] and not shown[h.id]:
                errors.append(f"{h.id} ({row_span(h.rows, 0, len(h.rows) - 1)}) is not shown; "
                              f"add {json.dumps({'diff': h.id})}")
                continue
            for lo, hi in runs:
                ref = {"diff": h.id, "range": f"rows:{lo + 1}-{hi + 1}"}
                errors.append(f"{h.id}: changed lines {row_span(h.rows, lo, hi)} are not shown; "
                              f"add {json.dumps(ref)}")
    main = [s for s in slides if s["section"] != "Appendix"]
    if sum(not s["quiz"] for s in main) > TRIVIAL and not main[-1]["quiz"]:
        errors.append(f"the deck has more than {TRIVIAL} content slides, so the last slide before the appendix "
                      "must be a final check of two to four questions that span the deck")
    if errors:
        raise DeckInvalid(errors)

    generated = _appendix_slides(groups) if groups else []
    coverage = None
    if diffs:
        diff_hunks = [h for f in files for h in f.hunks]
        in_appendix: set[tuple[str, int]] = set()
        for g in groups:
            for b in g["blocks"]:
                for p in b["pieces"]:
                    in_appendix.update((p["hunk"], i) for i in range(p["lo"], p["hi"] + 1))
        coverage = {
            "changed": sum(len(h.needed()) for h in diff_hunks),
            "appendix": sum((h.id, i) in in_appendix for h in diff_hunks for i in h.needed()),
            "files": len(files),
        }
    return Deck(deck_dir, spec, files + list(plain.values()), hunks, slides + generated, coverage, sources)


# Math delimiters, matched as the page matches them (pandoc's dollar rule).
# What is left after removing code and well-formed math is unbalanced.
CODE_RE = re.compile(r"^ {0,3}(`{3,}|~{3,}).*?(?:^ {0,3}\1 *$|\Z)|(`+).+?\2", re.M | re.S)
MATH_RE = re.compile(
    r"\$\$[^`]+?\$\$|\\\[[^`]+?\\\]|\\\([^`]+?\\\)|\$(?![\s$])(?:\\[^`]|[^\\$`])+?(?<!\s)\$(?!\d)|\\[\\$]"
)
STRAY_RE = re.compile(r"\$\$|\\[()\[\]]|\$(?=[A-Za-z\\{(])")


def markdown_fields(s: dict):
    """(where, text) for each Markdown field of a slide."""
    for b, blk in enumerate(s["blocks"]):
        if blk.get("md"):
            yield f"blocks[{b}]", blk["md"]
        if blk.get("caption"):
            yield f"blocks[{b}].caption", blk["caption"]
    for i, q in enumerate(s["quiz"]):
        yield f"quiz[{i}].q", q["q"]
        for j, c in enumerate(q["choices"]):
            yield f"quiz[{i}].choices[{j}].text", c["text"]
            yield f"quiz[{i}].choices[{j}].why", c["why"]
    for i, p in enumerate(s["prompts"]):
        yield f"prompts[{i}]", p


def math_warnings(deck: Deck) -> list[str]:
    out = []
    for s in deck.slides:
        for where, text in markdown_fields(s):
            rest = MATH_RE.sub(" ", CODE_RE.sub(" ", text))
            stray = sorted({m[0] for m in STRAY_RE.finditer(rest)})
            if stray:
                out.append(f"slide {s['id']!r}.{where}: unbalanced math delimiter {', '.join(stray)}")
    return out


def deck_payload(deck: Deck, version: int) -> dict:
    """The JSON the page renders."""
    files = []
    for f in deck.files:
        files.append(
            {
                "key": f.key,
                "label": f.label,
                "path": f.path,
                "oldPath": f.old_path,
                "status": f.status,
                "describe": f.describe(),
                "binary": f.binary,
                "meta": f.meta,
                "lang": f.language,
                "adds": f.adds,
                "dels": f.dels,
                "plain": f.plain,
                "hunks": [
                    {
                        "id": h.id,
                        "header": h.header,
                        "rows": [[r.kind, r.old, r.new, r.text] + ([1] if r.noeol else []) for r in h.rows],
                    }
                    for h in f.hunks
                ],
            }
        )
    spec = deck.spec
    diffs = [f for f in deck.files if not f.plain]
    return {
        "version": version,
        "title": spec.get("title", ""),
        "subtitle": spec.get("subtitle", ""),
        "scope": spec.get("scope", ""),
        "goal": spec.get("goal", ""),
        "sourceUrl": spec.get("source_url", ""),
        "stats": {
            "files": len(diffs),
            "adds": sum(f.adds for f in diffs),
            "dels": sum(f.dels for f in diffs),
        },
        "coverage": deck.coverage,
        "files": files,
        "slides": deck.slides,
    }


# --------------------------------------------------------------------------
# Questions
#
# The page posts a question and the server writes queue/<id>.json. `listen`
# hands the oldest unanswered question to the session, marks it
# claimed (<id>.claimed), and later writes the session's answer to
# <id>.answer.md, which the server moves into the slide's thread.


def refs_text(deck: Deck, refs: list[dict]) -> str:
    out = []
    for r in refs:
        if r["kind"] == "lines":
            h = deck.hunks[r["hunk"]]
            fence = (h.file.language or "text") if h.file.plain else "diff"
            out.append(
                f"Selected lines, {h.file.path} {span(h, r['lo'], r['hi'])}:\n"
                f"```{fence}\n{numbered(h, r['lo'], r['hi'])}\n```"
            )
        elif r["kind"] == "quote":
            where = QUOTE_SOURCES[r["source"]]
            out.append(f"Quoted from {where}:\n> " + r["text"].replace("\n", "\n> "))
    return "\n\n".join(out)


def write_atomic(path: Path, text: str, mode: int = 0o644):
    tmp = path.with_name(f".{path.name}.{os.getpid()}.{threading.get_ident()}.tmp")
    fd = os.open(tmp, os.O_WRONLY | os.O_CREAT | os.O_TRUNC, mode)
    with os.fdopen(fd, "w") as f:
        f.write(text)
    tmp.replace(path)


def pid_alive(pid) -> bool:
    try:
        os.kill(int(pid), 0)
        return True
    except (OSError, ValueError, TypeError):
        return False


class Queue:
    def __init__(self, deck_dir: Path):
        self.dir = deck_dir / "queue"

    def file(self, qid: str, suffix: str = ".json") -> Path:
        return self.dir / f"{qid}{suffix}"

    def add(self, item: dict):
        self.dir.mkdir(exist_ok=True)
        write_atomic(self.file(item["id"]), json.dumps(item, indent=1))

    def items(self) -> list[dict]:
        out = []
        for p in self.dir.glob("q-*.json"):
            try:
                out.append(json.loads(p.read_text()))
            except (OSError, ValueError):
                continue
        return sorted(out, key=lambda x: x.get("at", 0))

    def claimed(self, qid: str) -> bool:
        return self.file(qid, ".claimed").exists()

    def answered(self, qid: str) -> bool:
        return self.file(qid, ".answer.md").exists()

    def remove(self, qid: str):
        for suffix in (".json", ".claimed", ".answer.md"):
            self.file(qid, suffix).unlink(missing_ok=True)

    def listener(self) -> dict | None:
        try:
            info = json.loads((self.dir / "listening.json").read_text())
        except (OSError, ValueError):
            return None
        return info if pid_alive(info.get("pid")) and info.get("until", 0) > time.time() else None

    def pages(self) -> dict:
        try:
            return json.loads((self.dir / "pages.json").read_text())
        except (OSError, ValueError):
            return {}


# --------------------------------------------------------------------------
# Server


class QuizAnswers:
    """The reader's answers on check slides, persisted to quiz.json as
    {slide id: {question index: choice index}}. Only the server writes it."""

    def __init__(self, path: Path):
        self.path = path
        self.lock = threading.Lock()
        try:
            self.data = json.loads(path.read_text()).get("answers", {})
        except (OSError, ValueError):
            self.data = {}

    def set(self, slide: str, question: int, choice: int | None):
        with self.lock:
            answers = self.data.setdefault(slide, {})
            if choice is None:
                answers.pop(str(question), None)
            else:
                answers[str(question)] = choice
            write_atomic(self.path, json.dumps({"version": 1, "answers": self.data}, indent=1))

    def snapshot(self) -> dict:
        with self.lock:
            return json.loads(json.dumps(self.data))


def _plain(md: str, limit: int = 160) -> str:
    text = re.sub(r"[*`_>#]+", "", " ".join(md.split()))
    return text if len(text) <= limit else text[: limit - 1] + "…"


def quiz_report(deck: Deck, answers: dict) -> str:
    """What the reader got right and missed on the check slides, for the session."""
    checks = deck.checks()
    if not checks:
        return ""
    total = right = 0
    missed = []
    for s in checks:
        got = answers.get(s["id"], {})
        for i, q in enumerate(s["quiz"]):
            total += 1
            c = got.get(str(i))
            if not isinstance(c, int) or not 0 <= c < len(q["choices"]):
                continue
            if q["choices"][c]["correct"]:
                right += 1
                continue
            see = deck.slide(q["choices"][c]["see"] or q["see"])
            correct = next(x for x in q["choices"] if x["correct"])
            missed.append(
                f"  {s['title']}, question {i + 1}: {_plain(q['q'])}\n"
                f"    chose: {_plain(q['choices'][c]['text'])}\n"
                f"    correct: {_plain(correct['text'])}\n"
                + (f"    covered on {deck.position(see)}: {see['title']}" if see else "")
            )
    left = total - right - len(missed)
    head = f"Checks: {right} of {total} answered correctly" + (f", {left} unanswered" if left else "") + "."
    return "\n".join([head, *missed])


class Threads:
    """Per-slide chat history, persisted to threads.json. Only the server writes it."""

    def __init__(self, path: Path):
        self.path = path
        self.lock = threading.Lock()
        try:
            self.data = json.loads(path.read_text()).get("threads", {})
        except (OSError, ValueError):
            self.data = {}

    def save(self):
        write_atomic(self.path, json.dumps({"version": 1, "threads": self.data}, indent=1))

    def messages(self, slide: str) -> list[dict]:
        return self.data.setdefault(slide, {"messages": []})["messages"]

    def find(self, qid: str) -> tuple[str, dict] | tuple[None, None]:
        for slide, t in self.data.items():
            for m in t["messages"]:
                if m.get("id") == qid and m["role"] == "user":
                    return slide, m
        return None, None

    def snapshot(self) -> dict:
        with self.lock:
            return json.loads(json.dumps({k: v for k, v in self.data.items() if v["messages"]}))


class State:
    def __init__(self, deck_dir: Path, token: str):
        self.deck_dir = deck_dir
        self.token = token
        self.queue = Queue(deck_dir)
        self.queue.dir.mkdir(exist_ok=True)
        self.threads = Threads(deck_dir / "threads.json")
        self.quiz = QuizAnswers(deck_dir / "quiz.json")
        self.deck = load_deck(deck_dir)
        self.version = 1
        self.deck_errors: list[str] = []
        self.mtimes = self._mtimes()
        self.listeners: list[queue.Queue] = []
        self.listeners_lock = threading.Lock()
        self.presence = "away"
        self.last_listener = 0.0
        self.busy = False
        self.last_activity = time.monotonic()
        self.last_page = time.time()

    def status(self) -> dict:
        return {"session": self.presence}

    def payload(self) -> dict:
        return {**deck_payload(self.deck, self.version), "status": self.status()}

    def _mtimes(self) -> dict[Path, int]:
        out = {}
        for p in self.deck.sources:
            try:
                out[p] = p.stat().st_mtime_ns
            except OSError:
                out[p] = 0
        return out

    def broadcast(self, event: str, data: dict):
        with self.listeners_lock:
            for q in self.listeners:
                q.put((event, data))

    def pages_changed(self):
        with self.listeners_lock:
            n = len(self.listeners)
            if n:
                self.last_page = time.time()
            write_atomic(self.queue.dir / "pages.json", json.dumps({"open": n, "last_seen": self.last_page}))

    def watch(self):
        """Pick up deck edits, claims and answers from the session."""
        tick = 0
        while True:
            time.sleep(0.5)
            tick += 1
            try:
                self._sync_queue()
            except OSError as e:
                log(f"queue: {e}")
            now = time.time()
            if self.queue.listener():
                self.last_listener = now
            if now - self.last_listener < LISTEN_GRACE:
                presence = "listening"
            else:
                presence = "answering" if self.busy else "away"
            if presence != self.presence:
                self.presence = presence
                self.broadcast("status", self.status())
            if tick % 60 == 0:
                self.pages_changed()
            if tick % 2 == 0:
                self._reload_deck()

    def _reload_deck(self):
        if self._mtimes() == self.mtimes:
            return
        time.sleep(0.3)  # let a multi-file edit settle
        try:
            deck = load_deck(self.deck_dir)
        except DeckInvalid as e:
            self.mtimes = self._mtimes()
            self.deck_errors = e.errors
            self.broadcast("deck-error", {"errors": e.errors})
            log(f"deck edit rejected: {len(e.errors)} error(s)")
            return
        self.deck, self.deck_errors = deck, []
        self.version += 1
        self.mtimes = self._mtimes()
        self.broadcast("deck", {"version": self.version})
        log(f"deck reloaded (version {self.version})")

    def _sync_queue(self):
        changed = set()
        busy = False
        for item in self.queue.items():
            qid = item["id"]
            if item.get("kind") != "question":
                continue
            claim = self.queue.file(qid, ".claimed")
            try:
                busy |= time.time() - claim.stat().st_mtime < CLAIM_STALE
            except OSError:
                pass
            with self.threads.lock:
                slide, msg = self.threads.find(qid)
                if msg is None:
                    continue
                if self.queue.answered(qid):
                    text = self.queue.file(qid, ".answer.md").read_text()
                    msg["state"] = "answered"
                    self.threads.messages(slide).append(
                        {"id": qid + "-a", "role": "assistant", "text": text, "at": time.time()}
                    )
                    self.threads.save()
                    self.queue.remove(qid)
                    changed.add(slide)
                elif self.queue.claimed(qid) and msg.get("state") == "queued":
                    msg["state"] = "claimed"
                    self.threads.save()
                    changed.add(slide)
        self.busy = busy
        for slide in changed:
            self.broadcast("thread", {"slide": slide})


def log(msg: str):
    print(time.strftime("%H:%M:%S"), msg, file=sys.stderr, flush=True)


class ClientGone(Exception):
    pass


class Handler(BaseHTTPRequestHandler):
    server_version = "teach-me"
    state: State

    def log_message(self, fmt, *args):
        pass

    def _route(self) -> str | None:
        parts = urlsplit(self.path).path.split("/", 2)
        if len(parts) < 2 or not secrets.compare_digest(parts[1], self.state.token):
            return None
        return "/" + (parts[2] if len(parts) > 2 else "")

    def _send(self, status: int, body: bytes, ctype: str):
        self.send_response(status)
        self.send_header("Content-Type", ctype)
        self.send_header("Content-Length", str(len(body)))
        self.send_header("Cache-Control", "no-store")
        self.send_header("Referrer-Policy", "no-referrer")
        self.end_headers()
        self.wfile.write(body)

    def _json(self, data, status: int = 200):
        self._send(status, json.dumps(data).encode(), "application/json")

    def _error(self, status: int, message: str):
        self._json({"error": message}, status)

    def _body(self) -> dict:
        n = int(self.headers.get("Content-Length") or 0)
        if n > 1_000_000:
            raise ValueError("request too large")
        data = json.loads(self.rfile.read(n) or b"{}")
        if not isinstance(data, dict):
            raise ValueError("expected a JSON object")
        return data

    def _sse(self, event: str | None, data=None):
        try:
            frame = f"event: {event}\ndata: {json.dumps(data)}\n\n" if event else ": ping\n\n"
            self.wfile.write(frame.encode())
            self.wfile.flush()
        except OSError as e:
            raise ClientGone() from e

    def do_GET(self):
        route = self._route()
        if route is None:
            return self._error(404, "not found")
        if urlsplit(self.path).path == "/" + self.state.token:
            self.send_response(301)
            self.send_header("Location", f"/{self.state.token}/")
            self.end_headers()
            return
        st = self.state
        st.last_activity = time.monotonic()
        if route in ("/", "/index.html"):
            return self._static("index.html")
        if route == "/api/deck":
            return self._json(st.payload())
        if route == "/api/threads":
            return self._json(st.threads.snapshot())
        if route == "/api/quiz":
            return self._json(st.quiz.snapshot())
        if route == "/api/events":
            return self._events()
        name = route.lstrip("/")
        if "/" not in name and not name.startswith(".") and (ASSETS / name).is_file():
            return self._static(name)
        self._error(404, "not found")

    def _static(self, name: str):
        types = {".html": "text/html", ".css": "text/css", ".js": "text/javascript"}
        ctype = types.get(Path(name).suffix, "application/octet-stream") + "; charset=utf-8"
        self._send(200, (ASSETS / name).read_bytes(), ctype)

    def _events(self):
        q: queue.Queue = queue.Queue()
        st = self.state
        with st.listeners_lock:
            st.listeners.append(q)
        st.pages_changed()
        try:
            self.send_response(200)
            self.send_header("Content-Type", "text/event-stream")
            self.send_header("Cache-Control", "no-store")
            self.end_headers()
            self._sse("hello", {"version": st.version, "status": st.status()})
            if st.deck_errors:
                self._sse("deck-error", {"errors": st.deck_errors})
            while True:
                try:
                    event, data = q.get(timeout=HEARTBEAT)
                    self._sse(event, data)
                except queue.Empty:
                    self._sse(None)
        except ClientGone:
            pass
        finally:
            with st.listeners_lock:
                st.listeners.remove(q)
            st.pages_changed()

    def do_POST(self):
        route = self._route()
        if route is None:
            return self._error(404, "not found")
        origin = self.headers.get("Origin")
        if origin and urlsplit(origin).netloc != self.headers.get("Host"):
            return self._error(403, "cross-origin request refused")
        self.state.last_activity = time.monotonic()
        try:
            body = self._body()
        except ValueError as e:
            return self._error(400, str(e))
        handler = {
            "/api/ask": self._ask,
            "/api/withdraw": self._withdraw,
            "/api/clear": self._clear,
            "/api/answer": self._answer,
            "/api/finish": self._finish,
        }.get(route)
        if handler is None:
            return self._error(404, "not found")
        handler(body)

    def _refs(self, deck: Deck, refs) -> list[dict]:
        out = []
        for r in refs if isinstance(refs, list) else []:
            if not isinstance(r, dict):
                continue
            if r.get("kind") == "lines" and r.get("hunk") in deck.hunks:
                h = deck.hunks[r["hunk"]]
                try:
                    lo, hi = sorted((int(r.get("lo", 0)), int(r.get("hi", 0))))
                except (TypeError, ValueError):
                    continue
                lo, hi = max(lo, 0), min(hi, len(h.rows) - 1)
                label = f"{Path(h.file.path).name} {span(h, lo, hi)}"
                out.append({"kind": "lines", "hunk": h.id, "lo": lo, "hi": hi, "label": label})
            elif r.get("kind") in ("quote", "diagram") and isinstance(r.get("text"), str) and r["text"].strip():
                text = r["text"].strip()[:2000]
                source = r.get("source") if r.get("source") in QUOTE_SOURCES else "diagram"
                label = " ".join(text.split())
                label = (label if source == "math" else re.sub(r"[*`_]+", "", label))[:60]
                out.append({"kind": "quote", "source": source, "text": text, "label": label})
        return out[:8]

    def _ask(self, body: dict):
        st = self.state
        deck = st.deck
        slide = deck.slide(str(body.get("slide")))
        text = body.get("text")
        refs = self._refs(deck, body.get("refs"))
        if slide is None or not isinstance(text, str) or not (text.strip() or refs):
            return self._error(400, "expected a slide id and a question")
        qid = f"q-{int(time.time() * 1000):x}-{secrets.token_hex(2)}"
        msg = {"id": qid, "role": "user", "text": text.strip()[:20_000], "refs": refs,
               "at": time.time(), "state": "queued"}
        item = {
            "id": qid,
            "kind": "question",
            "slide": slide["id"],
            "where": f"{deck.position(slide)}: {slide['title']}",
            "text": msg["text"],
            "refs": refs_text(deck, refs),
            "at": msg["at"],
        }
        with st.threads.lock:
            st.threads.messages(slide["id"]).append(msg)
            st.threads.save()
            st.queue.add(item)
        st.broadcast("thread", {"slide": slide["id"]})
        self._json({"message": msg})

    def _withdraw(self, body: dict):
        st = self.state
        qid = str(body.get("id"))
        with st.threads.lock:
            slide, msg = st.threads.find(qid)
            if msg is None:
                return self._error(404, "no such question")
            if msg.get("state") != "queued" or st.queue.claimed(qid):
                return self._error(409, "the session is already answering it")
            st.queue.remove(qid)
            msg["state"] = "withdrawn"
            st.threads.save()
        st.broadcast("thread", {"slide": slide})
        self._json({"ok": True})

    def _clear(self, body: dict):
        st = self.state
        slide = str(body.get("slide"))
        with st.threads.lock:
            msgs = st.threads.data.get(slide, {}).get("messages", [])
            if any(m.get("state") in ("queued", "claimed") for m in msgs):
                return self._error(409, "a question on this slide is still waiting for an answer")
            st.threads.data.pop(slide, None)
            st.threads.save()
        st.broadcast("thread", {"slide": slide})
        self._json({"ok": True})

    def _answer(self, body: dict):
        st = self.state
        slide = st.deck.slide(str(body.get("slide")))
        question, choice = body.get("question"), body.get("choice")
        q = slide["quiz"][question] if slide and isinstance(question, int) and 0 <= question < len(slide["quiz"]) else None
        if q is None or not (choice is None or isinstance(choice, int) and 0 <= choice < len(q["choices"])):
            return self._error(400, "expected a check slide, a question index and a choice index")
        st.quiz.set(slide["id"], question, choice)
        st.broadcast("quiz", {"slide": slide["id"]})
        self._json({"ok": True})

    def _finish(self, body: dict):
        st = self.state
        qid = f"q-{int(time.time() * 1000):x}-{secrets.token_hex(2)}"
        st.queue.add({"id": qid, "kind": "finished", "at": time.time()})
        self._json({"ok": True})


# --------------------------------------------------------------------------
# Commands


def print_report(deck: Deck):
    main = deck.main()
    appendix = len(deck.slides) - len(main)
    head = f"{len(main)} slides and {appendix} appendix slides"
    c = deck.coverage
    if c:
        head += (f"; all {c['changed']} non-blank changed lines in {c['files']} files are shown "
                 f"({c['appendix']} only in the appendix)")
    print(head)
    if deck.spec.get("goal"):
        print(f"goal: {deck.spec['goal']}")
    checks = deck.checks()
    if not checks:
        print("no check slides; add a final check unless the deck is trivial")
    elif not main or not main[-1]["quiz"]:
        print(f"{len(checks)} check slide(s); the last authored slide is not a check")
    else:
        print(f"{len(checks)} check slide(s), ending with a final check")
    unchecked = [s["id"] for s in deck.unchecked()]
    seen_by = defaultdict(list)
    for chk in checks:
        for q in chk["quiz"]:
            for x in {q["see"], *(ch["see"] for ch in q["choices"])} - {""}:
                seen_by[x].append(chk["id"])
    print("\nslides:")
    for n, s in enumerate(deck.slides, 1):
        if s["quiz"]:
            note = "check"
        elif s["section"] == "Appendix":
            note = "appendix"
        else:
            note = f"checked by {', '.join(dict.fromkeys(seen_by[s['id']]))}" if s["id"] not in unchecked else "NOT CHECKED"
        print(f"  {n:>2}. {s['id']}: {s['title']}  [{note}]")
    # Without checks every slide is unchecked; the line above already says so.
    if checks and unchecked:
        print(
            f"\nwarning: {len(unchecked)} content slide(s) are not the `see` of any check question and not in "
            f"the appendix: {', '.join(unchecked)}. Point a question at each one the "
            "goal depends on, move the rest to the appendix, or leave a slide that needs no check."
        )
    math = math_warnings(deck)
    if math:
        print("\nwarning: math delimiters without a match; close the formula, or write a literal dollar as \\$:")
        for w in math:
            print(f"  {w}")


def cmd_hunks(args):
    deck_dir = Path(args.deck)
    diffs = args.diff or []
    if not diffs:
        try:
            spec = json.loads((deck_dir / "deck.json").read_text())
            # As in load_deck: a declared empty list means no diffs.
            for d in spec.get("diffs", ["change.diff"]) or []:
                d = {"file": d} if isinstance(d, str) else d
                diffs.append(d.get("file", "") + (f"={d['label']}" if d.get("label") else ""))
        except (OSError, ValueError):
            diffs = ["change.diff"]
    files: list[FileDiff] = []
    for d in diffs:
        name, _, label = d.partition("=")
        try:
            files += parse_diff((deck_dir / name).read_text(errors="replace"), label)
        except (OSError, DiffError) as e:
            sys.exit(f"{name}: {e}")
    print(
        f"{len(files)} files, {sum(len(f.hunks) for f in files)} hunks, "
        f"+{sum(f.adds for f in files)} −{sum(f.dels for f in files)}\n"
    )
    for f in files:
        print(f"{f.key}  ({f.describe()}, +{f.adds} −{f.dels})")
        if not f.hunks:
            print(f"  {f.key}  file-level change: {' '.join(f.meta) or f.describe()}")
        for h in f.hunks:
            print(f"  {h.id}  {h.header}  changed {', '.join(changed_spans(h, h.changed()))}")
            if args.show:
                for i, r in enumerate(h.rows, 1):
                    o = "" if r.old is None else r.old
                    n = "" if r.new is None else r.new
                    print(f"    {i:>4} {o:>5} {n:>5} {r.kind}{r.text}")


def cmd_check(args):
    try:
        deck = load_deck(Path(args.deck))
    except DeckInvalid as e:
        for err in e.errors:
            print(f"error: {err}")
        sys.exit(1)
    print_report(deck)


def server_info(deck_dir: Path) -> dict | None:
    try:
        info = json.loads((deck_dir / "server.json").read_text())
    except (OSError, ValueError):
        return None
    return info if pid_alive(info.get("pid")) else None


def cmd_serve(args):
    deck_dir = Path(args.deck).resolve()
    running = server_info(deck_dir)
    if running:
        print(f"already serving: {running['url']}")
        return
    if args.detach:
        argv = [a for a in sys.argv[1:] if a != "--detach"]
        with open(deck_dir / "server.log", "ab") as logf:
            proc = subprocess.Popen(
                [sys.executable, str(SCRIPT), *argv],
                stdin=subprocess.DEVNULL,
                stdout=logf,
                stderr=logf,
                start_new_session=True,
            )
        for _ in range(150):
            time.sleep(0.1)
            info = server_info(deck_dir)
            if info and info["pid"] == proc.pid:
                print(info["url"])
                return
            if proc.poll() is not None:
                break
        sys.exit(f"the server did not start; see {deck_dir / 'server.log'}")

    # Reuse the previous token and port, so a restarted server keeps the URL
    # and open pages reconnect on their own.
    saved = deck_dir / ".server-address"
    try:
        token, port = json.loads(saved.read_text()).values()
    except (OSError, ValueError):
        token, port = secrets.token_urlsafe(18), 0
    try:
        state = State(deck_dir, token)
    except DeckInvalid as e:
        for err in e.errors:
            print(f"error: {err}", file=sys.stderr)
        sys.exit(1)
    Handler.state = state
    try:
        httpd = ThreadingHTTPServer((args.host, args.port or port), Handler)
    except OSError:
        httpd = ThreadingHTTPServer((args.host, args.port), Handler)
    httpd.daemon_threads = True
    host, port = httpd.server_address[:2]
    shown = args.public_host or (socket.gethostname() if host in ("0.0.0.0", "::") else host)
    url = f"http://{shown}:{port}/{state.token}/"
    write_atomic(deck_dir / "server.json", json.dumps({"pid": os.getpid(), "url": url, "port": port}), 0o600)
    write_atomic(saved, json.dumps({"token": state.token, "port": port}), 0o600)
    state.pages_changed()
    print_report(state.deck)
    log(f"serving {url}")
    threading.Thread(target=state.watch, daemon=True).start()

    def idle():
        while True:
            time.sleep(60)
            with state.listeners_lock:
                open_pages = len(state.listeners)
            if not open_pages and time.monotonic() - state.last_activity > args.idle_hours * 3600:
                log("idle; shutting down")
                httpd.shutdown()
                return

    threading.Thread(target=idle, daemon=True).start()
    signal.signal(signal.SIGTERM, lambda *_: threading.Thread(target=httpd.shutdown).start())
    try:
        httpd.serve_forever()
    except KeyboardInterrupt:
        pass
    finally:
        (deck_dir / "server.json").unlink(missing_ok=True)


def cmd_stop(args):
    deck_dir = Path(args.deck).resolve()
    info = server_info(deck_dir)
    if not info:
        (deck_dir / "server.json").unlink(missing_ok=True)
        print("not running")
        return
    try:
        # Servers started as review_deck.py also match.
        if b"deck.py" not in Path(f"/proc/{info['pid']}/cmdline").read_bytes():
            sys.exit(f"pid {info['pid']} is not a deck server; remove {deck_dir / 'server.json'}")
    except OSError:
        pass  # no /proc (macOS); trust server.json
    os.kill(info["pid"], signal.SIGTERM)
    for _ in range(50):
        time.sleep(0.1)
        if not server_info(deck_dir):
            print("stopped")
            return
    sys.exit("the server did not stop")


def _ago(ts: float) -> str:
    minutes = int((time.time() - ts) // 60)
    return "just now" if minutes < 1 else f"{minutes} min ago"


def cmd_listen(args):
    deck_dir = Path(args.deck).resolve()
    q = Queue(deck_dir)
    me = shlex_path(SCRIPT)
    deck_arg = shlex_path(deck_dir)
    if args.answer:
        if not QUESTION_ID.match(args.answer) or not q.file(args.answer).exists():
            sys.exit(f"no open question {args.answer} (answered or withdrawn already?)")
        text = Path(args.file).read_text() if args.file else sys.stdin.read()
        if not text.strip():
            sys.exit("the answer is empty")
        write_atomic(q.file(args.answer, ".answer.md"), text)
        print(f"Posted the answer to {args.answer}.")
    if not server_info(deck_dir):
        sys.exit(f"No server is running for this deck. Start it with:\n  python3 {me} serve {deck_arg} --detach")

    q.dir.mkdir(exist_ok=True)
    marker = q.dir / "listening.json"
    deadline = time.time() + args.timeout
    write_atomic(marker, json.dumps({"pid": os.getpid(), "until": deadline + 5}))
    item = None
    try:
        while True:
            # Claimed but unanswered questions come first: a session that lost
            # track of one (an interruption, a context reset) gets it again.
            pending = [x for x in q.items() if x["id"] != args.answer and not q.answered(x["id"])]
            pending.sort(key=lambda x: (not q.claimed(x["id"]), x.get("at", 0)))
            if pending:
                item = pending[0]
                break
            if time.time() >= deadline:
                break
            time.sleep(0.5)
    finally:
        try:
            if json.loads(marker.read_text()).get("pid") == os.getpid():
                marker.unlink()
        except (OSError, ValueError):
            pass

    wait = "" if args.timeout == 100 else f" --timeout {args.timeout:g}"
    again = f"python3 {me} listen {deck_arg}{wait}"
    if item is None:
        pages = q.pages()
        if pages.get("open"):
            seen = f"The deck is open in {pages['open']} page(s)."
        else:
            seen = f"No page has the deck open (last seen {_ago(pages.get('last_seen', 0))})."
        print(f"No questions in the last {args.timeout:g} s. {seen}\nKeep listening with:\n  {again}")
        return
    rest = len(pending) - 1
    if item.get("kind") == "finished":
        q.remove(item["id"])
        print("The reader marked the walkthrough finished.")
        try:
            report = quiz_report(load_deck(deck_dir), QuizAnswers(deck_dir / "quiz.json").snapshot())
        except DeckInvalid:
            report = ""
        if report:
            print(report + "\nRevisit what was missed in the wrap-up.")
        if rest:
            print(f"{rest} question(s) still wait for answers; keep listening with:\n  {again}")
        return
    write_atomic(q.file(item["id"], ".claimed"), json.dumps({"pid": os.getpid(), "at": time.time()}))
    print(f"Question {item['id']} on slide {item['slide']} ({item['where']}):\n")
    if item.get("refs"):
        print(item["refs"] + "\n")
    print(item["text"] or "(No text; the reader wants the selection explained.)")
    print(
        "\nAnswer for the slide's chat panel: concise Markdown, citing path:line. Read what you"
        " need; change the deck only if the reader asks for that. Write the answer to a file,"
        " then post it and keep listening:\n"
        f"  python3 {me} listen {deck_arg} --answer {item['id']} --file ANSWER.md{wait}"
    )
    if rest:
        print(f"({rest} more question(s) waiting.)")


def shlex_path(p: Path) -> str:
    s = str(p)
    return s if re.fullmatch(r"[\w./~+-]+", s) else "'" + s.replace("'", "'\\''") + "'"


def main(argv=None):
    p = argparse.ArgumentParser(
        description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter
    )
    sub = p.add_subparsers(dest="cmd", required=True)

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

    c = sub.add_parser("check", help="validate a deck and report its checks and coverage")
    c.add_argument("deck")
    c.set_defaults(func=cmd_check)

    s = sub.add_parser("serve", help="serve the deck as a local web page")
    s.add_argument("deck")
    s.add_argument("--host", default="127.0.0.1", help="address to listen on (default 127.0.0.1)")
    s.add_argument("--port", type=int, default=0, help="port (default: any free port)")
    s.add_argument("--public-host", help="host name to print in the URL")
    s.add_argument("--idle-hours", type=float, default=12, help="exit after this long unused")
    s.add_argument("--detach", action="store_true", help="run in the background; print the URL")
    s.set_defaults(func=cmd_serve)

    l = sub.add_parser("listen", help="wait for the reader's next question")
    l.add_argument("deck")
    l.add_argument("--answer", metavar="ID", help="first post the answer to this question")
    l.add_argument("--file", help="answer file (default: read the answer from stdin)")
    l.add_argument("--timeout", type=float, default=100, help="seconds to wait (default 100)")
    l.set_defaults(func=cmd_listen)

    t = sub.add_parser("stop", help="stop a detached server")
    t.add_argument("deck")
    t.set_defaults(func=cmd_stop)

    args = p.parse_args(argv)
    args.func(args)


if __name__ == "__main__":
    main()
