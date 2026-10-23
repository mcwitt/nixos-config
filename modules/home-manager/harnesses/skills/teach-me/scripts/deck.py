#!/usr/bin/env python3
"""Check and serve a teaching slideshow.

`check` validates a deck and reports which slides its checks cover. `serve`
publishes the deck as a local web page with a chat panel on every slide.
`listen` hands the reader's questions to the session and posts its answers
back; `stop` ends a detached server.

A deck directory holds `deck.json` and any text it saved for excerpts. The
server writes `threads.json` (the chat history), `quiz.json` (check answers),
`server.json` (the running server), `.server-address` (the token, port and
options a restart reuses), `server.log` and a `queue/` directory next to them.
Questions, answers and the page's reports cross between the server and
`listen` only as files in `queue/`, so `listen` needs no network access.
Requires Python >= 3.11; uses only the standard library.

Another skill can build on this one with an Extension: a script of its own
that imports this module, registers the extension and calls main(). The
extension adds slide blocks, generated slides, server routes, `listen` items,
commands and page assets; see the Extension class.
"""

from __future__ import annotations

import argparse
import contextlib
import functools
import getpass
import hashlib
import json
import os
import queue
import re
import secrets
import shutil
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
# The script the user runs: this one, or an extension's, which detached
# servers and printed commands must use.
ENTRY = SCRIPT
SLIDE_ID = re.compile(r"^[A-Za-z][A-Za-z0-9._~-]{0,63}$")
QUESTION_ID = re.compile(r"^q-[0-9a-z-]+$")
QUOTE_SOURCES = {"diagram": "a diagram", "callout": "a callout", "text": "the slide's text", "quiz": "a check",
                 "math": "a formula"}
LINES_RE = re.compile(r"^(\d+)(?:-(\d+))?$")
HEARTBEAT = 15.0
# The session re-runs `listen` after each timeout; treat it as listening for
# this long after the last run so the page does not flicker between runs.
LISTEN_GRACE = 90.0
CLAIM_STALE = 20 * 60


@functools.cache
def engine() -> str:
    """A digest of the scripts and page assets, the extensions' included: a deploy changes it."""
    h = hashlib.sha256()
    for p in [SCRIPT, ENTRY, *asset_files()]:
        h.update(p.name.encode() + b"\0" + p.read_bytes())
    return h.hexdigest()[:12]


def asset_files() -> list[Path]:
    """The page's files, the core's and the extensions'; they share one namespace, so names must differ."""
    files = [p for d in [ASSETS, *(e.assets for e in EXTENSIONS if e.assets)] for p in sorted(d.iterdir())]
    names = [p.name for p in files]
    if len(set(names)) != len(names):
        raise SystemExit(f"page assets with the same name: {sorted({n for n in names if names.count(n) > 1})}")
    return files


# --------------------------------------------------------------------------
# Extensions


@dataclass
class Loading:
    """What load_deck has read so far, for the extensions' hooks."""
    dir: Path
    spec: dict
    errors: list[str]
    sources: list[Path]
    files: list[FileDiff] = field(default_factory=list)  # files an extension adds, such as diffs
    hunks: dict[str, Hunk] = field(default_factory=dict)
    slides: list[dict] = field(default_factory=list)
    extra: dict = field(default_factory=dict)  # becomes Deck.extra
    goal_optional: bool = False


class Extension:
    """A layer over the core. Every hook's default does nothing."""

    name = ""
    assets: Path | None = None  # page assets; the page loads NAME.css and NAME.js from here
    blocks: tuple[str, ...] = ()  # slide block keys it handles, beside md, callout, excerpt and diagram
    spec_keys: tuple[str, ...] = ()  # deck.json keys it reads, beside the core's

    def begin(self, load: Loading):
        """Before the slides: read its parts of deck.json."""

    def block(self, load: Loading, key: str, blk: dict, where: str, out: list[dict]):
        """Validate a block with one of its keys and append it to `out`."""

    def reserves(self, slide_id: str) -> bool:
        """Whether a slide id is kept for a slide it generates."""
        return False

    def end(self, load: Loading) -> list[dict]:
        """After the slides: check the deck as a whole, and return the slides it adds."""
        return []

    def payload(self, deck: Deck) -> dict:
        """Fields it adds to the JSON the page renders."""
        return {}

    def report(self, deck: Deck) -> str:
        """What `check` says about the deck after the slide counts."""
        return ""

    def attach(self, state: State):
        """When the server starts."""

    def tick(self, state: State):
        """Twice a second on the server's watcher thread."""

    def get(self, state: State, route: str):
        """A GET route's JSON, or None when the route is not its."""

    def post(self, state: State, route: str, body: dict):
        """A POST route's JSON, or None when the route is not its; ValueError answers 400."""

    def skip(self, q: Queue, item: dict) -> bool:
        """Whether `listen` should drop a queued item that no longer needs the session."""
        return False

    def listen(self, item: dict, ctx: Listening) -> bool:
        """Print a queued item of its own for the session; return whether it did."""
        return False

    def finished(self, ctx: Listening) -> str:
        """What `listen` adds when the reader finishes."""
        return ""

    def commands(self, sub):
        """Add its subcommands to the argument parser."""


EXTENSIONS: list[Extension] = []


@dataclass
class Listening:
    """What `listen` passes to the extensions: where the deck is and how to call this script."""
    deck_dir: Path
    q: Queue
    me: str  # the script, quoted for a shell
    deck_arg: str
    again: str  # the command that keeps listening


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


def row_lines(rows: list[Row], lo: int, hi: int) -> tuple[str, int, int] | None:
    """Rows lo..hi as (side, first line, last line): the new side unless they are all removed lines."""
    news = [r.new for r in rows[lo : hi + 1] if r.new is not None]
    olds = [r.old for r in rows[lo : hi + 1] if r.old is not None]
    side, nums = ("new", news) if news else ("old", olds)
    return (side, nums[0], nums[-1]) if nums else None


def row_span(rows: list[Row], lo: int, hi: int) -> str:
    """Describe rows lo..hi by line numbers, e.g. `new 12–18` or `old 4`."""
    lines = row_lines(rows, lo, hi)
    if not lines:
        return "?"
    side, first, last = lines
    return f"{side} {first}" if first == last else f"{side} {first}–{last}"


def span(h: Hunk, lo: int, hi: int) -> str:
    """Rows lo..hi as a reader sees them: `new 12–18` in a diff, `12–18` in a plain excerpt."""
    s = row_span(h.rows, lo, hi)
    return s.removeprefix("new ") if h.file.plain else s


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
# Slides are free-form. A deck past TRIVIAL content slides must end with a
# check, and a deck must state its goal unless an extension says otherwise.

QUIZ_QUESTIONS = (1, 6)  # questions per check slide
QUIZ_CHOICES = (2, 5)  # choices per question
WRITTEN = ("prose", "code")  # question kinds the session judges
VERDICTS = {"correct": "Correct", "partial": "Partly right", "incorrect": "Not yet"}
TRIVIAL = 3  # content slides a deck may have without a final check


class DeckInvalid(Exception):
    def __init__(self, errors: list[str]):
        super().__init__("\n".join(errors))
        self.errors = errors


@dataclass
class Deck:
    dir: Path
    spec: dict
    files: list[FileDiff]  # an extension's files, such as diffs, then plain excerpt sources
    hunks: dict[str, Hunk]
    slides: list[dict]  # authored slides, then the extensions' generated ones
    sources: list[Path]
    extra: dict = field(default_factory=dict)  # what the extensions keep, by key

    def slide(self, slide_id: str) -> dict | None:
        return next((s for s in self.slides if s["id"] == slide_id), None)

    def position(self, s: dict) -> str:
        where = f"slide {self.slides.index(s) + 1} of {len(self.slides)}"
        return f"{where}, {s['section']}" if s["section"] else where

    def checks(self) -> list[dict]:
        return [s for s in self.slides if s["quiz"]]

    def main(self) -> list[dict]:
        """The authored slides outside the appendix."""
        return [s for s in self.slides if s["section"] != "Appendix" and not s.get("generated")]

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


def _quiz(quiz, where: str, sid: str, targets: set[str], deck_dir: Path, errors: list[str]) -> list[dict]:
    """Normalize a check slide's questions; `targets` are the content slide ids."""
    lo, hi = QUIZ_QUESTIONS
    if not isinstance(quiz, list) or not lo <= len(quiz) <= hi:
        errors.append(f"{where}.quiz: expected a list of {lo} to {hi} questions")
        return []
    out = []
    for i, q in enumerate(quiz):
        qw = f"{where}.quiz[{i}]"
        if not isinstance(q, dict):
            errors.append(f"{qw}: expected {{q, see, choices}} or {{q, see, kind, rubric}}")
            continue
        kind = q.get("kind", "choice")
        item = {
            "q": _str(q.get("q"), f"{qw}.q", errors, True),
            "see": _see(q.get("see"), f"{qw}.see", sid, targets, errors, True),
            "kind": kind,
            "choices": [],
        }
        if kind == "choice":
            _choices(q.get("choices"), item, qw, sid, targets, errors)
        elif kind in WRITTEN:
            if "choices" in q:
                errors.append(f"{qw}: a {kind} question has a rubric, not choices")
            item["rubric"] = _str(q.get("rubric"), f"{qw}.rubric", errors, True)
            item["model"] = _str(q.get("model"), f"{qw}.model", errors)
            if kind == "code":
                item["language"] = _str(q.get("language"), f"{qw}.language", errors)
                item["starter"] = _str(q.get("starter"), f"{qw}.starter", errors)
                item["test"] = _test(q.get("test"), f"{qw}.test", deck_dir, errors)
        else:
            errors.append(f"{qw}.kind: expected choice, prose or code")
        out.append(item)
    return out


def _choices(choices, item: dict, qw: str, sid: str, targets: set[str], errors: list[str]):
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


def _test(t, where: str, deck_dir: Path, errors: list[str]) -> dict | None:
    """A code question's test harness: `command` runs in a copy of `dir` holding the answer as `file`."""
    if t is None:
        return None
    if not isinstance(t, dict):
        errors.append(f"{where}: expected {{file, command, dir, timeout}}")
        return None
    out = {
        "file": _str(t.get("file"), f"{where}.file", errors, True),
        "command": _str(t.get("command"), f"{where}.command", errors, True),
        "dir": _str(t.get("dir"), f"{where}.dir", errors),
        "timeout": t.get("timeout", 60),
    }
    if out["file"] and (Path(out["file"]).name != out["file"] or out["file"] in (".", "..")):
        errors.append(f"{where}.file: expected a file name without a directory")
    test_dir = (deck_dir / out["dir"]).resolve()
    if out["dir"] and not test_dir.is_dir():
        errors.append(f"{where}.dir: {test_dir} is not a directory")
    elif out["dir"] and (test_dir == deck_dir.resolve() or not test_dir.is_relative_to(deck_dir.resolve())
                         or test_dir.is_relative_to(deck_dir.resolve() / "runs")):
        errors.append(f"{where}.dir: expected a subdirectory of the deck directory, outside runs/, where the tests run")
    if not isinstance(out["timeout"], (int, float)) or not 0 < out["timeout"] <= 600:
        errors.append(f"{where}.timeout: expected seconds, at most 600")
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
    known = {"title", "subtitle", "scope", "goal", "root", "source_url", "slides", *(k for e in EXTENSIONS for k in e.spec_keys)}
    for key in sorted(set(spec) - known):
        errors.append(f"{key}: not a key this script reads (a deck for another skill's script?)")
    load = Loading(deck_dir, spec, errors, [spec_path])
    for ext in EXTENSIONS:
        ext.begin(load)
    handlers = {key: ext for ext in EXTENSIONS for key in ext.blocks}
    plain: dict[str, FileDiff] = {}
    root = deck_dir / _str(spec.get("root"), "root", errors)

    _str(spec.get("title"), "title", errors, True)
    for key in ("subtitle", "scope", "source_url"):
        _str(spec.get(key), key, errors)
    goal = spec.get("goal")
    if not load.goal_optional or goal is not None:
        _str(goal, "goal", errors, True)
    slides_in = spec.get("slides")
    if not isinstance(slides_in, list) or not slides_in:
        errors.append("slides: expected a non-empty list")
        slides_in = []

    keys = {"md", "callout", "excerpt", "diagram", *handlers}
    seen_ids: set[str] = set()
    slides = load.slides
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
        elif any(ext.reserves(sid) for ext in EXTENSIONS):
            errors.append(f"{where}.id: {sid!r} is reserved for a generated slide")
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
            out["quiz"] = _quiz(s["quiz"], where, sid, content_ids, deck_dir, errors)
            slides.append(out)
            continue
        blocks = s.get("blocks")
        if not isinstance(blocks, list) or not blocks:
            errors.append(f"{where}.blocks: expected a non-empty list")
            blocks = []
        for b, blk in enumerate(blocks):
            bw = f"{where}.blocks[{b}]"
            found = keys & set(blk if isinstance(blk, dict) else ())
            if len(found) != 1:
                errors.append(f"{bw}: expected exactly one of {', '.join(sorted(keys))}")
                continue
            (key,) = found
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
                ex = _plain_excerpt(blk, bw, root, plain, load.hunks, load.sources, errors)
                if ex:
                    out["blocks"].append({"type": "excerpt", **ex, "caption": _str(blk.get("caption"), f"{bw}.caption", errors)})
            else:
                handlers[key].block(load, key, blk, bw, out["blocks"])
        slides.append(out)

    generated = [g for ext in EXTENSIONS for g in ext.end(load)]
    main = [s for s in slides if s["section"] != "Appendix"]
    if sum(not s["quiz"] for s in main) > TRIVIAL and not main[-1]["quiz"]:
        errors.append(f"the deck has more than {TRIVIAL} content slides, so the last slide before the appendix "
                      "must be a final check of two to four questions that span the deck")
    if errors:
        raise DeckInvalid(errors)
    return Deck(deck_dir, spec, load.files + list(plain.values()), load.hunks, slides + generated, load.sources, load.extra)


# Math delimiters, matched as the page matches them (pandoc's dollar rule).
# What is left after removing code and well-formed math is unbalanced.
CODE_RE = re.compile(r"^ {0,3}(`{3,}|~{3,}).*?(?:^ {0,3}\1 *$|\Z)|(`+).+?\2", re.M | re.S)
MATH_RE = re.compile(
    r"\$\$[^`]+?\$\$|\\\[[^`]+?\\\]|\\\([^`]+?\\\)|\$(?![\s$])(?:\\[^`]|[^\\$`])+?(?<!\s)\$(?!\d)|\\[\\$]"
)
STRAY_RE = re.compile(r"\$\$|\\[()\[\]]|\$(?=[A-Za-z\\{(])")


def markdown_fields(s: dict):
    """(where, text) for each Markdown field of a slide."""
    def blocks(bs: list[dict], where: str):
        for b, blk in enumerate(bs):
            if blk.get("md"):
                yield f"{where}[{b}]", blk["md"]
            if blk.get("caption"):
                yield f"{where}[{b}].caption", blk["caption"]
            yield from blocks(blk.get("blocks", []), f"{where}[{b}].blocks")

    yield from blocks(s["blocks"], "blocks")
    for i, q in enumerate(s["quiz"]):
        yield f"quiz[{i}].q", q["q"]
        if q["kind"] == "prose" and q["model"]:
            yield f"quiz[{i}].model", q["model"]
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


def file_payload(f: FileDiff) -> dict:
    return {
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
            {"id": h.id, "header": h.header, "rows": [[r.kind, r.old, r.new, r.text] + ([1] if r.noeol else []) for r in h.rows]}
            for h in f.hunks
        ],
    }


def deck_payload(deck: Deck, version: int) -> dict:
    """The JSON the page renders."""
    spec = deck.spec
    out = {
        "version": version,
        "title": spec.get("title", ""),
        "subtitle": spec.get("subtitle", ""),
        "scope": spec.get("scope", ""),
        "goal": spec.get("goal", ""),
        "sourceUrl": spec.get("source_url", ""),
        "files": [file_payload(f) for f in deck.files],
        "slides": deck.slides,
    }
    for ext in EXTENSIONS:
        out.update(ext.payload(deck))
    return out


# --------------------------------------------------------------------------
# Questions
#
# The page posts a question and the server writes queue/<id>.json. `listen`
# hands the oldest unanswered question to the session, marks it
# claimed (<id>.claimed), and later writes the session's answer to
# <id>.answer.md, which the server moves into the slide's thread.


def refs_text(hunks: dict[str, Hunk], refs: list[dict]) -> str:
    out = []
    for r in refs:
        if r["kind"] == "lines":
            h = hunks[r["hunk"]]
            fence = (h.file.language or "text") if h.file.plain else "diff"
            out.append(
                f"Selected lines, {h.file.path} {span(h, r['lo'], r['hi'])}:\n"
                f"```{fence}\n{numbered(h, r['lo'], r['hi'])}\n```"
            )
        elif r["kind"] == "quote":
            where = QUOTE_SOURCES[r["source"]]
            out.append(f"Quoted from {where}:\n> " + r["text"].replace("\n", "\n> "))
        elif r["kind"] == "place":
            out.append(f"At {r['path']}:{r['line']}" + (f"-{r['end']}" if r["end"] != r["line"] else ""))
    return "\n\n".join(out)


def parse_refs(hunks: dict[str, Hunk], refs) -> list[dict]:
    """Validate references: selected lines and quotes from a page, and places from the session."""
    out = []
    for r in refs if isinstance(refs, list) else []:
        if not isinstance(r, dict):
            continue
        if r.get("kind") == "lines" and r.get("hunk") in hunks:
            h = hunks[r["hunk"]]
            try:
                lo, hi = sorted((int(r.get("lo", 0)), int(r.get("hi", 0))))
            except (TypeError, ValueError):
                continue
            lo, hi = max(lo, 0), min(hi, len(h.rows) - 1)
            side, line, end = row_lines(h.rows, lo, hi) or ("new", 0, 0)
            # path, side, line and end locate the lines outside the deck.
            out.append({"kind": "lines", "hunk": h.id, "lo": lo, "hi": hi, "path": h.file.path, "side": side,
                        "line": line, "end": end, "label": f"{Path(h.file.path).name} {span(h, lo, hi)}"})
        elif r.get("kind") in ("quote", "diagram") and isinstance(r.get("text"), str) and r["text"].strip():
            source = r.get("source") if r.get("source") in QUOTE_SOURCES else "diagram"
            out.append({"kind": "quote", "source": source, "text": r["text"].strip()[:2000]})
        elif r.get("kind") == "place" and isinstance(r.get("path"), str) and isinstance(r.get("line"), int):
            end = r.get("end") if isinstance(r.get("end"), int) and r["end"] >= r["line"] else r["line"]
            out.append({"kind": "place", "path": r["path"], "side": "new", "line": r["line"], "end": end,
                        "label": f"{Path(r['path']).name} {r['line']}" + (f"–{end}" if end != r["line"] else "")})
    return out[:8]


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


def read_json(path: Path) -> dict:
    """A JSON object from a file, or {} when it is missing or unreadable."""
    try:
        data = json.loads(path.read_text())
    except (OSError, ValueError):
        return {}
    return data if isinstance(data, dict) else {}


def new_id(prefix: str = "q") -> str:
    return f"{prefix}-{int(time.time() * 1000):x}-{secrets.token_hex(2)}"


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

    def claim_age(self, qid: str) -> float:
        """Seconds since the session claimed an item, or infinity when it has not."""
        try:
            return time.time() - self.file(qid, ".claimed").stat().st_mtime
        except OSError:
            return float("inf")

    def answered(self, qid: str) -> bool:
        return self.file(qid, ".answer.md").exists()

    def remove(self, qid: str):
        for suffix in (".json", ".claimed", ".answer.md", ".verdict"):
            self.file(qid, suffix).unlink(missing_ok=True)

    def listener(self) -> dict | None:
        info = read_json(self.dir / "listening.json")
        return info if pid_alive(info.get("pid")) and info.get("until", 0) > time.time() else None

    def pages(self) -> dict:
        return read_json(self.dir / "pages.json")


# --------------------------------------------------------------------------
# Server


class QuizAnswers:
    """The reader's answers on check slides, persisted to quiz.json as
    {slide id: {question index: answer}}. An answer is a choice index or, for
    a written question, {id, text, at, state, verdict, feedback}, where state
    is queued, judging or judged. Only the server writes it."""

    def __init__(self, path: Path):
        self.path = path
        self.lock = threading.Lock()
        self.data = read_json(path).get("answers", {})

    def _save(self):
        write_atomic(self.path, json.dumps({"version": 1, "answers": self.data}, indent=1))

    def set(self, slide: str, question: int, answer):
        with self.lock:
            answers = self.data.setdefault(slide, {})
            if answer is None:
                answers.pop(str(question), None)
            else:
                answers[str(question)] = answer
            self._save()

    def get(self, slide: str, question: int):
        with self.lock:
            return json.loads(json.dumps(self.data.get(slide, {}).get(str(question))))

    def update(self, slide: str, question: int, qid: str, **fields) -> bool:
        """Update the written answer `qid`, unless the reader has replaced it."""
        with self.lock:
            a = self.data.get(slide, {}).get(str(question))
            if not isinstance(a, dict) or a.get("id") != qid:
                return False
            a.update(fields)
            self._save()
            return True

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
    total = right = waiting = 0
    missed = []
    for s in checks:
        got = answers.get(s["id"], {})
        for i, q in enumerate(s["quiz"]):
            total += 1
            a = got.get(str(i))
            head = f"  {s['title']}, question {i + 1}: {_plain(q['q'])}\n"
            if q["kind"] == "choice":
                if not isinstance(a, int) or not 0 <= a < len(q["choices"]):
                    continue
                if q["choices"][a]["correct"]:
                    right += 1
                    continue
                correct = next(x for x in q["choices"] if x["correct"])
                detail = f"    chose: {_plain(q['choices'][a]['text'])}\n    correct: {_plain(correct['text'])}\n"
                see = deck.slide(q["choices"][a]["see"] or q["see"])
            else:
                if not isinstance(a, dict):
                    continue
                if a.get("state") != "judged":
                    waiting += 1
                    continue
                if a.get("verdict") == "correct":
                    right += 1
                    continue
                detail = f"    answered ({VERDICTS.get(a.get('verdict'), 'judged').lower()}): {_plain(a['text'])}\n"
                see = deck.slide(q["see"])
            missed.append(head + detail + (f"    covered on {deck.position(see)}: {see['title']}" if see else ""))
    left = total - right - len(missed) - waiting
    head = f"Checks: {right} of {total} answered correctly"
    head += "".join(f", {n} {what}" for n, what in ((waiting, "waiting to be judged"), (left, "unanswered")) if n)
    return "\n".join([head + ".", *missed])


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
    def __init__(self, deck: Deck, token: str):
        deck_dir = self.deck_dir = deck.dir
        self.token = token
        self.queue = Queue(deck_dir)
        self.queue.dir.mkdir(exist_ok=True)
        self.threads = Threads(deck_dir / "threads.json")
        self.quiz = QuizAnswers(deck_dir / "quiz.json")
        self.deck = deck
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
        # Problems a page found drawing this deck version, as (slide, where, message).
        self.audited = 0
        self.issues: set[tuple[str, str, str]] = set()
        self.issues_lock = threading.Lock()
        for ext in EXTENSIONS:
            ext.attach(self)

    def status(self) -> dict:
        return {"session": self.presence}

    def payload(self) -> dict:
        return {**deck_payload(self.deck, self.version), "status": self.status(),
                "engine": engine(), "audited": self.audited == self.version}

    def report_issues(self, version: int, issues: list[tuple[str, str, str]], audit: bool):
        """Queue the problems no page has reported yet for `listen` to pass on;
        `audit` says a page drew every slide, so later pages need not."""
        with self.issues_lock:
            if version != self.version:
                return
            if audit:
                self.audited = version
            new = [i for i in dict.fromkeys(issues) if i not in self.issues and self.deck.slide(i[0])]
            self.issues.update(new)
            lines = [
                {"where": f"{self.deck.position(self.deck.slide(sid))} ({sid}), {where}", "message": message}
                for sid, where, message in new
            ]
        if not new:
            return
        self.queue.add({"id": new_id(), "kind": "issues", "at": time.time(), "issues": lines})
        log(f"page reported {len(new)} problem(s) drawing the deck")

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
                for ext in EXTENSIONS:
                    ext.tick(self)
            except Exception as e:  # keep watching; the log says what failed
                log(f"queue: {e!r}")
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
        with self.issues_lock:
            self.deck, self.deck_errors = deck, []
            self.version += 1
            self.issues.clear()
        self.mtimes = self._mtimes()
        self.broadcast("deck", {"version": self.version})
        log(f"deck reloaded (version {self.version})")

    def _sync_queue(self):
        changed = set()
        busy = False
        for item in self.queue.items():
            qid = item["id"]
            if item.get("kind") not in ("question", "answer"):
                continue
            busy |= self.queue.claim_age(qid) < CLAIM_STALE
            if item["kind"] == "answer":
                self._sync_answer(item)
                continue
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

    def _sync_answer(self, item: dict):
        """Mark a written answer as being judged, or record the session's verdict."""
        qid, slide, question = item["id"], item["slide"], item["question"]
        if self.queue.answered(qid):
            verdict = self.queue.file(qid, ".verdict")
            verdict = verdict.read_text().strip() if verdict.exists() else ""
            feedback = self.queue.file(qid, ".answer.md").read_text()
            verdict = verdict if verdict in VERDICTS else None
            self.queue.remove(qid)
            changed = self.quiz.update(slide, question, qid, state="judged", verdict=verdict, feedback=feedback)
        elif self.queue.claimed(qid):
            # A deck edit can leave a choice index where the written answer was.
            a = self.quiz.get(slide, question)
            changed = isinstance(a, dict) and a.get("state") == "queued" and self.quiz.update(slide, question, qid, state="judging")
        else:
            changed = False
        if changed:
            self.broadcast("quiz", {"slide": slide})


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
        for ext in EXTENSIONS:
            data = ext.get(st, route)
            if data is not None:
                return self._json(data)
        name = route.lstrip("/")
        path = next((p for p in asset_files() if p.name == name), None)
        if path and not name.startswith("."):
            return self._static(name)
        self._error(404, "not found")

    def _static(self, name: str):
        types = {".html": "text/html", ".css": "text/css", ".js": "text/javascript"}
        ctype = types.get(Path(name).suffix, "application/octet-stream") + "; charset=utf-8"
        body = next(p for p in asset_files() if p.name == name).read_bytes()
        if name == "index.html":
            # The extensions' styles and scripts load after the core's.
            tags = "".join(f'<link rel="stylesheet" href="{e.name}.css">\n<script src="{e.name}.js" defer></script>\n'
                           for e in EXTENSIONS if e.assets)
            body = body.replace(b"<!-- extensions -->\n", tags.encode())
        self._send(200, body, ctype)

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
            self._sse("hello", {"version": st.version, "status": st.status(), "engine": engine()})
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
            "/api/issues": self._issues,
        }.get(route)
        if handler is None:
            for ext in EXTENSIONS:
                try:
                    data = ext.post(self.state, route, body)
                except ValueError as e:
                    return self._error(400, str(e))
                if data is not None:
                    return self._json(data)
            return self._error(404, "not found")
        handler(body)

    def _ask(self, body: dict):
        st = self.state
        deck = st.deck
        slide = deck.slide(str(body.get("slide")))
        text = body.get("text")
        refs = parse_refs(deck.hunks, body.get("refs"))
        if slide is None or not isinstance(text, str) or not (text.strip() or refs):
            return self._error(400, "expected a slide id and a question")
        qid = new_id()
        msg = {"id": qid, "role": "user", "text": text.strip()[:20_000], "refs": refs,
               "at": time.time(), "state": "queued"}
        item = {
            "id": qid,
            "kind": "question",
            "slide": slide["id"],
            "where": f"{deck.position(slide)}: {slide['title']}",
            "text": msg["text"],
            "refs": refs_text(deck.hunks, refs),
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
        """Record a choice, submit a written answer for judging, or (with neither) clear an answer."""
        st = self.state
        slide = st.deck.slide(str(body.get("slide")))
        question, choice, text = body.get("question"), body.get("choice"), body.get("text")
        q = slide["quiz"][question] if slide and isinstance(question, int) and 0 <= question < len(slide["quiz"]) else None
        if q is None:
            return self._error(400, "expected a check slide and a question index")
        sid = slide["id"]
        if q["kind"] == "choice":
            if not (choice is None or isinstance(choice, int) and 0 <= choice < len(q["choices"])):
                return self._error(400, "expected a choice index")
            st.quiz.set(sid, question, choice)
        else:
            prev = st.quiz.get(sid, question)
            if isinstance(prev, dict) and prev.get("state") in ("queued", "judging"):
                if st.queue.claim_age(prev["id"]) < CLAIM_STALE:
                    return self._error(409, "the session is judging this answer")
                st.queue.remove(prev["id"])
            if text is None:
                st.quiz.set(sid, question, None)
            elif not isinstance(text, str) or not text.strip():
                return self._error(400, "expected the answer's text")
            else:
                qid, now = new_id(), time.time()
                st.quiz.set(sid, question, {"id": qid, "text": text[:50_000], "state": "queued", "at": now})
                st.queue.add({"id": qid, "kind": "answer", "slide": sid, "question": question, "at": now,
                              "where": f"{st.deck.position(slide)}: {slide['title']}", "text": text[:50_000], "spec": q})
        st.broadcast("quiz", {"slide": sid})
        self._json({"ok": True})

    def _issues(self, body: dict):
        st = self.state
        issues = []
        for i in body.get("issues") if isinstance(body.get("issues"), list) else []:
            if isinstance(i, dict) and isinstance(i.get("message"), str):
                issues.append((str(i.get("slide")), str(i.get("where", ""))[:80], i["message"][:500]))
        version = body.get("version")
        st.report_issues(version if isinstance(version, int) else -1, issues[:50], body.get("audit") is True)
        self._json({"ok": True})

    def _finish(self, body: dict):
        st = self.state
        st.queue.add({"id": new_id(), "kind": "finished", "at": time.time()})
        self._json({"ok": True})


# --------------------------------------------------------------------------
# Commands


def print_report(deck: Deck):
    main = deck.main()
    appendix = sum(s["section"] == "Appendix" for s in deck.slides)
    head = f"{len(main)} slides and {appendix} appendix slides"
    print(head + "".join(ext.report(deck) for ext in EXTENSIONS))
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
        elif s.get("generated"):
            note = "generated"
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


def load_or_exit(deck_dir: Path) -> Deck:
    try:
        return load_deck(deck_dir)
    except DeckInvalid as e:
        for err in e.errors:
            print(f"error: {err}")
        sys.exit(1)


def cmd_check(args):
    print_report(load_or_exit(Path(args.deck)))


def server_info(deck_dir: Path) -> dict | None:
    info = read_json(deck_dir / "server.json")
    return info if pid_alive(info.get("pid")) else None


# Options a deck directory keeps between servers, with their defaults.
SERVE_OPTIONS = {"host": "127.0.0.1", "port": 0, "public_host": None, "idle_hours": 12.0, "tunnel": None}


def serve_options(args, saved: dict) -> dict:
    """The saved options, overridden by the flags given on this command line."""
    given = {k: getattr(args, k) for k in SERVE_OPTIONS if getattr(args, k) is not None}
    opts = {**SERVE_OPTIONS, **saved, **given}
    if opts["tunnel"] == "":
        opts["tunnel"] = f"{getpass.getuser()}@{socket.gethostname()}"
    elif opts["tunnel"] == "off":
        opts["tunnel"] = None
    return opts


def stale_reason(info: dict) -> str | None:
    """Why a running server is not this deck.py with these page assets, if it is not."""
    if info.get("engine") == engine():
        return None
    if info.get("script") == str(ENTRY):
        return "deck.py or its page changed since it started"
    return f"it runs {info.get('script', 'an older deck.py')}"


def print_address(info: dict):
    print(info["url"])
    if info.get("tunnel"):
        print(f"From another machine, forward the port first: {info['tunnel']}")


def stop_server(deck_dir: Path, info: dict):
    try:
        # Servers started as review_deck.py also match.
        if b"deck.py" not in Path(f"/proc/{info['pid']}/cmdline").read_bytes():
            sys.exit(f"pid {info['pid']} is not a deck server; remove {deck_dir / 'server.json'}")
    except OSError:
        pass  # no /proc (macOS); trust server.json
    os.kill(info["pid"], signal.SIGTERM)
    # Wait for the process to exit, not just for server.json to go: older servers removed it before freeing the port.
    for _ in range(50):
        time.sleep(0.1)
        if not pid_alive(info["pid"]):
            (deck_dir / "server.json").unlink(missing_ok=True)
            return
    sys.exit("the server did not stop")


def cmd_serve(args):
    deck_dir = Path(args.deck).resolve()
    saved = deck_dir / ".server-address"
    address = read_json(saved)
    opts = serve_options(args, address.get("options", {}))
    if opts["tunnel"] and opts["host"] not in ("127.0.0.1", "localhost", "::1"):
        sys.exit("--tunnel forwards a loopback port; leave out --host, or pass --tunnel off")
    running = server_info(deck_dir)
    # A current server keeps serving the last valid deck while deck.json has errors.
    if running and not stale_reason(running) and running.get("options") == opts:
        print("already serving:")
        print_address(running)
        return
    deck = load_or_exit(deck_dir)
    if running:
        # Restart in place: the token and port carry over, so open pages reconnect and reload.
        stop_server(deck_dir, running)
        print(f"restarted the server: {stale_reason(running) or 'new options'}")
    if args.detach:
        argv = [a for a in sys.argv[1:] if a != "--detach"]
        with open(deck_dir / "server.log", "ab") as logf:
            proc = subprocess.Popen(
                [sys.executable, str(ENTRY), *argv],
                stdin=subprocess.DEVNULL,
                stdout=logf,
                stderr=logf,
                # Its own session: no terminal, so closing the terminal or tmux session sends it no SIGHUP.
                start_new_session=True,
            )
        for _ in range(150):
            time.sleep(0.1)
            info = server_info(deck_dir)
            if info and info["pid"] == proc.pid:
                print_address(info)
                return
            if proc.poll() is not None:
                break
        sys.exit(f"the server did not start; see {deck_dir / 'server.log'}")

    sys.stdout.reconfigure(line_buffering=True)  # keep server.log in order
    token = address.get("token") or secrets.token_urlsafe(18)
    state = State(deck, token)
    Handler.state = state
    # Reuse the previous port so the URL survives a restart, unless --port names one.
    try:
        httpd = ThreadingHTTPServer((opts["host"], opts["port"] or address.get("port", 0)), Handler)
    except OSError:
        httpd = ThreadingHTTPServer((opts["host"], opts["port"]), Handler)
    httpd.daemon_threads = True
    host, port = httpd.server_address[:2]
    shown = opts["public_host"] or (socket.gethostname() if host in ("0.0.0.0", "::") else host)
    info = {
        "pid": os.getpid(),
        "url": f"http://{shown}:{port}/{token}/",
        "port": port,
        "tunnel": f"ssh -N -L {port}:127.0.0.1:{port} {opts['tunnel']}" if opts["tunnel"] else None,
        "engine": engine(),
        "script": str(ENTRY),
        "options": opts,
    }
    write_atomic(deck_dir / "server.json", json.dumps(info), 0o600)
    write_atomic(saved, json.dumps({"token": token, "port": port, "options": opts}), 0o600)
    state.pages_changed()
    print_report(state.deck)
    print_address(info)
    log(f"serving {info['url']} (pid {os.getpid()}, engine {engine()} at {ENTRY})")
    threading.Thread(target=state.watch, daemon=True).start()

    def shutdown(why: str):
        log(f"{why}; shutting down")
        threading.Thread(target=httpd.shutdown).start()

    def idle():
        while True:
            time.sleep(60)
            with state.listeners_lock:
                open_pages = len(state.listeners)
            if not open_pages and time.monotonic() - state.last_activity > opts["idle_hours"] * 3600:
                return shutdown("idle")

    threading.Thread(target=idle, daemon=True).start()
    signal.signal(signal.SIGTERM, lambda *_: shutdown("received SIGTERM"))
    signal.signal(signal.SIGHUP, lambda *_: log("ignored SIGHUP"))
    try:
        httpd.serve_forever()
    except KeyboardInterrupt:
        log("interrupted")
    finally:
        # Free the port before a restart can see server.json gone and bind it.
        httpd.server_close()
        (deck_dir / "server.json").unlink(missing_ok=True)
        log("stopped")


def cmd_stop(args):
    deck_dir = Path(args.deck).resolve()
    info = server_info(deck_dir)
    if not info:
        (deck_dir / "server.json").unlink(missing_ok=True)
        print("not running")
        return
    stop_server(deck_dir, info)
    print("stopped")


def _ago(ts: float) -> str:
    minutes = int((time.time() - ts) // 60)
    return "just now" if minutes < 1 else f"{minutes} min ago"


def cmd_listen(args):
    deck_dir = Path(args.deck).resolve()
    q = Queue(deck_dir)
    me = shlex_path(ENTRY)
    deck_arg = shlex_path(deck_dir)
    if args.answer:
        posted = read_json(q.file(args.answer)) if QUESTION_ID.match(args.answer) else {}
        if not posted:
            sys.exit(f"nothing open with id {args.answer} (answered, withdrawn or replaced already?)")
        judging = posted.get("kind") == "answer"
        if judging and args.verdict not in VERDICTS:
            sys.exit(f"{args.answer} is a check answer; pass --verdict {'|'.join(VERDICTS)}")
        if not judging and args.verdict:
            sys.exit(f"{args.answer} is a question; leave out --verdict")
        text = Path(args.file).read_text() if args.file else sys.stdin.read()
        if not text.strip():
            sys.exit("the answer is empty")
        if judging:
            write_atomic(q.file(args.answer, ".verdict"), args.verdict)
        write_atomic(q.file(args.answer, ".answer.md"), text)
        print(f"Posted the {'feedback' if judging else 'answer'} to {args.answer}.")
    info = server_info(deck_dir)
    serve = f"python3 {me} serve {deck_arg} --detach"
    if not info:
        sys.exit(f"No server is running for this deck. Start it with:\n  {serve}")
    if Path(info.get("script", ENTRY)).name != ENTRY.name:
        sys.exit(f"This deck is served by {info['script']}, which handles its requests; listen with that script.")
    if stale_reason(info):
        print(f"The server is out of date: {stale_reason(info)}. "
              f"Restart it in place; the URL stays the same and open pages reload:\n  {serve}\n")

    q.dir.mkdir(exist_ok=True)
    marker = q.dir / "listening.json"
    deadline = time.time() + args.timeout
    write_atomic(marker, json.dumps({"pid": os.getpid(), "until": deadline + 5}))
    item = None
    try:
        while True:
            # Claimed but unanswered questions come first: a session that lost
            # track of one (an interruption, a context reset) gets it again.
            pending = [x for x in q.items() if x["id"] != args.answer and not q.answered(x["id"])
                       and not any(ext.skip(q, x) for ext in EXTENSIONS)]
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
    ctx = Listening(deck_dir, q, me, deck_arg, again)
    if item is None:
        pages = q.pages()
        if pages.get("open"):
            seen = f"The deck is open in {pages['open']} page(s)."
        else:
            seen = f"No page has the deck open (last seen {_ago(pages.get('last_seen', 0))})."
        print(f"No questions in the last {args.timeout:g} s. {seen}\nKeep listening with:\n  {again}")
        return
    rest = len(pending) - 1
    if item.get("kind") == "issues":
        q.remove(item["id"])
        print("The page found problems drawing the deck. Fix them in deck.json; the page reloads it:")
        for i in item["issues"]:
            print(f"  {i['where']}: " + i["message"].replace("\n", "\n    "))
        print(f"\nThen keep listening with:\n  {again}")
        if rest:
            print(f"({rest} more waiting.)")
        return
    if any(ext.listen(item, ctx) for ext in EXTENSIONS):
        return
    if item.get("kind") == "finished":
        q.remove(item["id"])
        print("The reader marked the walkthrough finished.")
        try:
            report = quiz_report(load_deck(deck_dir), QuizAnswers(deck_dir / "quiz.json").snapshot())
        except DeckInvalid:
            report = ""
        if report:
            print(report + "\nRevisit what was missed in the wrap-up.")
        for ext in EXTENSIONS:
            extra = ext.finished(ctx)
            if extra:
                print(extra)
        if rest:
            print(f"{rest} more still wait for you; keep listening with:\n  {again}")
        return
    if item.get("kind") not in ("question", "answer"):
        sys.exit(f"{item['id']} is a {item.get('kind')!r} request this script cannot handle; listen with the deck's own script.")
    write_atomic(q.file(item["id"], ".claimed"), json.dumps({"pid": os.getpid(), "at": time.time()}))
    if item.get("kind") == "answer":
        print(judging_brief(item, deck_dir))
        print(f"  python3 {me} listen {deck_arg} --answer {item['id']} --verdict {'|'.join(VERDICTS)} "
              f"--file FEEDBACK.md{wait}")
        if rest:
            print(f"({rest} more waiting.)")
        return
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
        print(f"({rest} more waiting.)")


def fenced(text: str, lang: str = "") -> str:
    ticks = "`" * max([3, *(len(m) + 1 for m in re.findall(r"`{3,}", text))])
    return f"{ticks}{lang}\n{text.rstrip()}\n{ticks}"


def judging_brief(item: dict, deck_dir: Path) -> str:
    """What the session needs to judge a written check answer."""
    q = item["spec"]
    lang = q.get("language", "") if q["kind"] == "code" else ""
    out = [
        f"Answer {item['id']} to check question {item['question'] + 1} on slide {item['slide']} "
        f"({item['where']}), a {q['kind']} answer to judge.\n",
        f"Question:\n{q['q']}\n",
        f"Rubric:\n{q['rubric']}\n",
    ]
    if q.get("model"):
        out.append(f"Model answer (the page offers it after judging):\n{fenced(q['model'], lang) if lang else q['model']}\n")
    out.append(f"The reader's answer:\n{fenced(item['text'], lang)}\n")
    if q.get("test"):
        out.append(run_test(item, deck_dir) + "\n")
    out.append(
        "Judge the answer against the rubric, not its wording: correct when it has every point, partial when it"
        " has some, incorrect otherwise. Write feedback for the reader in Markdown: what is right, what is missing"
        " or wrong and where the deck covers it, without quoting the model answer. Then post it with the verdict"
        " and keep listening:"
    )
    return "\n".join(out)


def run_test(item: dict, deck_dir: Path) -> str:
    """Run a code answer's tests once, in a copy of the test directory, and describe the result."""
    t = item["spec"]["test"]
    run = deck_dir / "runs" / item["id"]
    report = run.with_suffix(".txt")
    if report.exists():
        return report.read_text()
    shutil.rmtree(run, ignore_errors=True)
    if t["dir"]:
        shutil.copytree(deck_dir / t["dir"], run)
    else:
        run.mkdir(parents=True)
    (run / t["file"]).write_text(item["text"])
    proc = subprocess.Popen(t["command"], shell=True, cwd=run, stdin=subprocess.DEVNULL, stdout=subprocess.PIPE,
                            stderr=subprocess.STDOUT, start_new_session=True)
    try:
        output, _ = proc.communicate(timeout=t["timeout"])
        status = "passed" if proc.returncode == 0 else f"failed with exit status {proc.returncode}"
    except subprocess.TimeoutExpired:
        with contextlib.suppress(ProcessLookupError):
            os.killpg(proc.pid, signal.SIGKILL)
        output, _ = proc.communicate()
        status = f"timed out after {t['timeout']:g} s"
    text = output.decode(errors="replace")
    if len(text) > 6000:
        text = "…\n" + text[-6000:]
    result = f"Tests ({t['command']}, in {run}) {status}:\n{fenced(text or '(no output)')}"
    write_atomic(report, result)
    return result


def shlex_path(p: Path) -> str:
    s = str(p)
    return s if re.fullmatch(r"[\w./~+-]+", s) else "'" + s.replace("'", "'\\''") + "'"


def main(argv=None, entry: str | None = None, description: str | None = None):
    """Run a command; an extension's script passes itself as `entry`, and its own help text."""
    global ENTRY
    if entry:
        ENTRY = Path(entry).resolve()
    p = argparse.ArgumentParser(
        description=description or __doc__, formatter_class=argparse.RawDescriptionHelpFormatter
    )
    sub = p.add_subparsers(dest="cmd", required=True)

    c = sub.add_parser("check", help="validate a deck and report its checks")
    c.add_argument("deck")
    c.set_defaults(func=cmd_check)

    s = sub.add_parser(
        "serve",
        help="serve the deck as a local web page",
        description="Serve the deck. The deck directory keeps its token, port and these options for later "
        "servers; a server running another deck.py or other options is restarted in place.",
    )
    s.add_argument("deck")
    s.add_argument("--host", help="address to listen on (default 127.0.0.1)")
    s.add_argument("--port", type=int, help="port (default: the previous one, or any free port)")
    s.add_argument("--public-host", help="host name to print in the URL")
    s.add_argument("--idle-hours", type=float, help="exit after this long unused (default 12)")
    s.add_argument(
        "--tunnel",
        nargs="?",
        const="",
        metavar="DEST",
        help="print the ssh command that forwards the port from the reader's machine, which reaches this "
        "one as ssh DEST (default USER@HOSTNAME); 'off' stops printing it",
    )
    s.add_argument("--detach", action="store_true", help="run in the background; print the URL")
    s.set_defaults(func=cmd_serve)

    l = sub.add_parser("listen", help="wait for the reader's next question")
    l.add_argument("deck")
    l.add_argument("--answer", metavar="ID", help="first post the answer to this question, or feedback on this check answer")
    l.add_argument("--file", help="answer file (default: read the answer from stdin)")
    l.add_argument("--verdict", choices=list(VERDICTS), help="with --answer for a check answer: the verdict")
    l.add_argument("--timeout", type=float, default=100, help="seconds to wait (default 100)")
    l.set_defaults(func=cmd_listen)

    for ext in EXTENSIONS:
        ext.commands(sub)

    t = sub.add_parser("stop", help="stop a detached server")
    t.add_argument("deck")
    t.set_defaults(func=cmd_stop)

    args = p.parse_args(argv)
    args.func(args)


if __name__ == "__main__":
    main()
