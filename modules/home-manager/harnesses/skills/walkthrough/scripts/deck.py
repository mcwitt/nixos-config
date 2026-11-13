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
import html
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
from collections.abc import Mapping
from pathlib import Path
from types import MappingProxyType
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
    quote_sources: Mapping[str, str] = MappingProxyType({})  # what a reader can quote, beside the core's: {source: "a description"}

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

    def busy(self, state: State) -> bool:
        """Whether the session is working on something the page asked of it, away from `listen`."""
        return False

    def reloaded(self, state: State, old: Deck):
        """After the server takes an edited deck; `old` is the deck it replaced."""

    def question(self, ctx: Listening) -> str:
        """What `listen` adds to every question it hands over."""
        return ""

    def hunks(self, state: State) -> dict[str, Hunk]:
        """Hunks beyond the deck's that the reader can quote."""
        return {}

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


def quote_sources() -> dict[str, str]:
    return {**QUOTE_SOURCES, **{k: v for e in EXTENSIONS for k, v in e.quote_sources.items()}}


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


# --------------------------------------------------------------------------
# Dials: audience and detail
#
# A deck is written at one audience and one detail stop (deck.json's
# `audience` and `detail`). The reader can ask for a content slide at another
# setting; the session rewrites it in place and stores it as
# variants/SLIDE/A-D.json. A variant records the key of the slide it was
# written from, so an edit to that slide, or to the lines it quotes, retires
# it. references/dials.md defines the stops.

AUDIENCE = ("novice", "newcomer", "practitioner", "specialist", "expert")
DETAIL = ("concise", "focused", "standard", "thorough", "complete")
DIALS = {"audience": AUDIENCE, "detail": DETAIL}
DIALS_DOC = SCRIPT.parent.parent / "references" / "dials.md"
VARIANT_NAME = re.compile(r"^([1-5])-([1-5])\.json$")


def parse_stop(value, dial: str) -> int:
    """A stop given as 1-5 or by name."""
    names = DIALS[dial]
    v = str(value).strip().lower()
    if v.isdigit() and 1 <= int(v) <= len(names):
        return int(v)
    if v in names:
        return names.index(v) + 1
    raise ValueError(f"{dial}: expected 1-5 or one of {', '.join(names)}, not {value!r}")


def stop_name(dial: str, n: int) -> str:
    return f"{n} ({DIALS[dial][n - 1]})"


def setting_text(a: int, d: int) -> str:
    return f"audience {stop_name('audience', a)}, detail {stop_name('detail', d)}"


@functools.cache
def stop_definition(dial: str, n: int) -> str:
    """The row for one stop in references/dials.md, as "name: definition"."""
    try:
        doc = DIALS_DOC.read_text()
    except OSError:
        return DIALS[dial][n - 1]
    m = re.search(rf"^\| {n} \| {DIALS[dial][n - 1]} \|(.*)\|\s*$", doc.split(f"## {dial.title()}", 1)[-1], re.M)
    cells = [c.strip() for c in m[1].split("|")] if m else []
    return f"{DIALS[dial][n - 1]}: " + " ".join(cells) if cells else DIALS[dial][n - 1]


def _setting(spec: dict, errors: list[str]) -> tuple[int, int] | None:
    """The deck's own setting; a deck that gives neither has none, and the page shows no dials."""
    if spec.get("audience") is None and spec.get("detail") is None:
        return None
    out = []
    for dial, default in (("audience", None), ("detail", 3)):
        v = spec.get(dial, default)
        try:
            if v is None:
                raise ValueError(f"{dial}: required beside {'detail' if dial == 'audience' else 'audience'}; "
                                 f"1-5 or one of {', '.join(DIALS[dial])}")
            out.append(parse_stop(v, dial))
        except ValueError as e:
            errors.append(str(e))
            out.append(3)
    return out[0], out[1]


@dataclass
class Variants:
    """The variants on disk: fresh ones by slide and setting, and what is wrong with the rest."""
    fresh: dict[str, dict[str, dict]] = field(default_factory=dict)  # slide id -> "A-D" -> slide
    stale: list[Path] = field(default_factory=list)
    invalid: list[Path] = field(default_factory=list)  # files that do not parse, or name no content slide
    errors: list[str] = field(default_factory=list)
    keys: dict[str, str] = field(default_factory=dict)  # slide id -> its current key


def _quoted(slide: dict, hunks: dict[str, Hunk]) -> list:
    """The lines a slide's excerpts show, as (kind, text) rows."""
    return [[[r.kind, r.text] for r in hunks[p["hunk"]].rows[p["lo"]:p["hi"] + 1]]
            for b in slide["blocks"] for blk in [b, *b.get("blocks", [])] for p in blk.get("pieces", [])]


def _digest(body) -> str:
    return hashlib.sha256(json.dumps(body, sort_keys=True).encode()).hexdigest()[:16]


def slide_key(slide: dict, hunks: dict[str, Hunk], spec: dict) -> str:
    """What a variant was written from: the slide as written, the lines it quotes, and the reader and goal."""
    return _digest({"slide": slide, "lines": _quoted(slide, hunks), "reader": spec.get("reader"), "goal": spec.get("goal")})


def lines_key(variant: dict, hunks: dict[str, Hunk]) -> str:
    """The lines a variant itself quotes, which can differ from the slide as written."""
    return _digest(_quoted(variant, hunks))


def variant_dials(slide: dict) -> bool:
    """Whether a slide takes variants: authored content and check slides, not generated ones."""
    return not slide.get("generated") and not any(ext.reserves(slide["id"]) for ext in EXTENSIONS)


def check_sees(slide: dict) -> list[str]:
    """The content slides a check's questions and choices point at."""
    return sorted({x for q in slide["quiz"] for x in (q["see"], *(ch["see"] for ch in q.get("choices", []))) if x})


def quiz_key(sid: str, setting: str, quiz: list[dict]) -> str:
    """Where the reader's answers to one version of a check are kept. It changes when a question is
    reworded, added or removed, or its right choice moves, and not for an edit to a choice's wording."""
    shape = [[q["q"], q["kind"], len(q.get("choices", [])), next((j for j, c in enumerate(q.get("choices", [])) if c["correct"]), -1)]
             for q in quiz]
    return f"{sid}@{setting}:{_digest(shape)[:8]}"


def _load_variants(load: Loading, slides: list[dict], content, check, setting, plain: dict) -> Variants:
    """Read DECK/variants/: parse each variant like a slide, and set aside stale and invalid ones.

    A variant is stale when the slide it was written from changed, or when the lines it quotes changed
    since it was stored. An invalid one adds no files or hunks to the deck."""
    out = Variants()
    vdir = load.dir / "variants"
    load.sources.append(vdir)
    if setting is None:
        return out
    by_id = {s["id"]: s for s in slides if variant_dials(s)}
    for sid, s in by_id.items():
        if not s["quiz"]:
            out.keys[sid] = slide_key(s, load.hunks, load.spec)
    # A check is written from the slides it covers, so it goes stale with them.
    for sid, s in by_id.items():
        if s["quiz"]:
            out.keys[sid] = _digest({"slide": s, "covers": [out.keys.get(x) for x in check_sees(s)],
                                     "reader": load.spec.get("reader"), "goal": load.spec.get("goal")})
    if not vdir.is_dir():
        return out
    for sub in sorted(p for p in vdir.iterdir() if p.is_dir()):
        load.sources.append(sub)
        for path in sorted(sub.glob("*.json")):
            load.sources.append(path)
            m = VARIANT_NAME.match(path.name)
            base = by_id.get(sub.name)
            where = f"variants/{sub.name}/{path.name}"
            if not m or base is None:
                out.errors.append(f"{where}: no content slide {sub.name!r}, or not named A-D.json")
                out.invalid.append(path)
                continue
            try:
                rec = json.loads(path.read_text())
            except (OSError, ValueError) as e:
                out.errors.append(f"{where}: {e}")
                out.invalid.append(path)
                continue
            if rec.get("key") != out.keys[sub.name]:
                out.stale.append(path)
                continue
            raw = rec.get("slide") if isinstance(rec.get("slide"), dict) else {}
            errs: list[str] = []
            before = (set(plain), set(load.hunks), {k: len(f.hunks) for k, f in plain.items()})
            load.errors, outer = errs, load.errors  # an extension's blocks report to load.errors
            v = {"id": base["id"], "title": _str(raw.get("title", base["title"]), f"{where}.title", errs, True),
                 "section": base["section"], "prompts": [], "blocks": [], "quiz": [],
                 "setting": {"audience": int(m[1]), "detail": int(m[2])}}
            try:
                if base["quiz"]:
                    check(raw, where, base["id"], v, errs)
                else:
                    content(raw, where, v, errs)
            finally:
                load.errors = outer
            if base["quiz"] and not errs:
                v["qkey"] = quiz_key(base["id"], f"{m[1]}-{m[2]}", v["quiz"])
            if errs or rec.get("lines", lines_key(v, load.hunks)) != lines_key(v, load.hunks):
                # Undo what the parse added, so that a rejected variant ships nothing in the payload.
                for k in set(plain) - before[0]:
                    del plain[k]
                for k in set(load.hunks) - before[1]:
                    del load.hunks[k]
                for k, f in plain.items():
                    del f.hunks[before[2][k]:]
                if errs:
                    out.errors.extend(errs)
                    out.invalid.append(path)
                else:
                    out.stale.append(path)
                continue
            out.fresh.setdefault(sub.name, {})[f"{m[1]}-{m[2]}"] = v
    return out


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
    out = {"type": kind, "caption": _str(d.get("caption"), f"{where}.caption", errors),
           "alt": _str(d.get("alt"), f"{where}.alt", errors)}
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
    known = {"title", "subtitle", "scope", "goal", "reader", "audience", "detail", "root", "source_url", "slides", *(k for e in EXTENSIONS for k in e.spec_keys)}
    for key in sorted(set(spec) - known):
        errors.append(f"{key}: not a key this script reads (a deck for another skill's script?)")
    load = Loading(deck_dir, spec, errors, [spec_path])
    for ext in EXTENSIONS:
        ext.begin(load)
    handlers = {key: ext for ext in EXTENSIONS for key in ext.blocks}
    plain: dict[str, FileDiff] = {}
    root = deck_dir / _str(spec.get("root"), "root", errors)

    _str(spec.get("title"), "title", errors, True)
    for key in ("subtitle", "scope", "reader", "source_url"):
        _str(spec.get(key), key, errors)
    goal = spec.get("goal")
    if not load.goal_optional or goal is not None:
        _str(goal, "goal", errors, True)
    slides_in = spec.get("slides")
    if not isinstance(slides_in, list) or not slides_in:
        errors.append("slides: expected a non-empty list")
        slides_in = []

    keys = {"md", "callout", "excerpt", "diagram", *handlers}

    def prompts(s: dict, where: str, out: dict, errors: list[str]):
        p = s.get("prompts") or []
        if not isinstance(p, list) or not all(isinstance(x, str) for x in p):
            errors.append(f"{where}.prompts: expected a list of strings")
        else:
            out["prompts"] = p

    def content(s: dict, where: str, out: dict, errors: list[str]):
        """Parse a content slide's prompts and blocks into `out`."""
        prompts(s, where, out, errors)
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
        if "quiz" in s:
            prompts(s, where, out, errors)
            if "blocks" in s:
                errors.append(f"{where}: a check slide has quiz, not blocks")
            out["quiz"] = _quiz(s["quiz"], where, sid, content_ids, deck_dir, errors)
        else:
            content(s, where, out, errors)
        slides.append(out)

    generated = [g for ext in EXTENSIONS for g in ext.end(load)]
    main = [s for s in slides if s["section"] != "Appendix"]
    if sum(not s["quiz"] for s in main) > TRIVIAL and not main[-1]["quiz"]:
        errors.append(f"the deck has more than {TRIVIAL} content slides, so the last slide before the appendix "
                      "must be a final check of two to four questions that span the deck")
    setting = _setting(spec, errors)
    def check(s: dict, where: str, sid: str, out: dict, errors: list[str]):
        """Parse a check slide's prompts and questions into `out`."""
        prompts(s, where, out, errors)
        if "blocks" in s or "quiz" not in s:
            errors.append(f"{where}: a check slide has quiz, not blocks")
        out["quiz"] = _quiz(s.get("quiz"), where, sid, content_ids, deck_dir, errors)

    base_setting = f"{setting[0]}-{setting[1]}" if setting else "as-written"
    for s in slides:
        if s["quiz"]:
            s["qkey"] = quiz_key(s["id"], base_setting, s["quiz"])
    variants = _load_variants(load, slides, content, check, setting, plain) if not errors else Variants()
    if errors:
        raise DeckInvalid(errors)
    load.extra["variants"] = variants
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
    setting = _setting(spec, [])
    if setting:
        variants: Variants = deck.extra["variants"]
        out["dials"] = {"audience": setting[0], "detail": setting[1], "stops": DIALS,
                        "slides": [s["id"] for s in deck.slides if variant_dials(s)], "variants": variants.fresh}
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
    sources = quote_sources()
    for r in refs:
        if r["kind"] == "lines":
            h = hunks[r["hunk"]]
            fence = (h.file.language or "text") if h.file.plain else "diff"
            out.append(
                f"Selected lines, {h.file.path} {span(h, r['lo'], r['hi'])}:\n"
                f"```{fence}\n{numbered(h, r['lo'], r['hi'])}\n```"
            )
        elif r["kind"] == "quote":
            where = sources[r["source"]]
            out.append(f"Quoted from {where}:\n> " + r["text"].replace("\n", "\n> "))
        elif r["kind"] == "place":
            out.append(f"At {r['path']}:{r['line']}" + (f"-{r['end']}" if r["end"] != r["line"] else ""))
    return "\n\n".join(out)


def parse_refs(hunks: dict[str, Hunk], refs) -> list[dict]:
    """Validate references: selected lines and quotes from a page, and places from the session."""
    out = []
    sources = quote_sources()
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
            source = r.get("source") if r.get("source") in sources else "diagram"
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
    {version: {question index: answer}}, where a version is a check's
    `qkey` (see quiz_key), so that each version of a check keeps its own
    answers. An answer is a choice index or, for a written question, {id,
    text, at, state, verdict, feedback}, where state is queued, judging or
    judged. `at` records when each version was last answered. Only the
    server writes it."""

    def __init__(self, path: Path):
        self.path = path
        self.lock = threading.Lock()
        saved = read_json(path)
        self.data = saved.get("answers", {})
        self.at: dict[str, float] = saved.get("at", {})

    def migrate(self, deck: Deck):
        """Move answers kept by slide id (before versions) to the check's version as written."""
        with self.lock:
            moved = False
            for s in deck.checks():
                if s["id"] in self.data and s["qkey"] not in self.data:
                    self.data[s["qkey"]] = self.data.pop(s["id"])
                    moved = True
            if moved:
                self._save()

    def _save(self):
        write_atomic(self.path, json.dumps({"version": 2, "answers": self.data, "at": self.at}, indent=1))

    def set(self, slide: str, question: int, answer):
        with self.lock:
            self.at[slide] = time.time()
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


def check_versions(deck: Deck, sid: str) -> list[dict]:
    """A check as written and its fresh rewrites at other settings."""
    base = deck.slide(sid)
    fresh = deck.extra.get("variants", Variants()).fresh.get(sid, {})
    return [base, *(fresh[k] for k in sorted(fresh))]


def quiz_report(deck: Deck, answers: dict, at: dict | None = None) -> str:
    """What the reader got right and missed on the check slides, for the session. A check that the
    reader answered at several settings counts at the one answered last."""
    checks = deck.checks()
    if not checks:
        return ""
    total = right = waiting = 0
    missed = []
    at = at or {}
    for base in checks:
        versions = check_versions(deck, base["id"])
        s = max(versions, key=lambda v: (v["qkey"] in answers, at.get(v["qkey"], 0), v is base))
        got = answers.get(s["qkey"], {})
        for i, q in enumerate(s["quiz"]):
            total += 1
            a = got.get(str(i))
            shown_at = f" (at {setting_text(*s['setting'].values())})" if s.get("setting") else ""
            head = f"  {s['title']}{shown_at}, question {i + 1}: {_plain(q['q'])}\n"
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
        self.quiz.migrate(deck)
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
        self.variant_requests: dict[str, dict[str, str]] = {}  # slide id -> "A-D" -> queued or claimed
        self.issues_lock = threading.Lock()
        for ext in EXTENSIONS:
            ext.attach(self)

    def status(self) -> dict:
        return {"session": self.presence}

    def payload(self) -> dict:
        out = {**deck_payload(self.deck, self.version), "status": self.status(),
               "engine": engine(), "audited": self.audited == self.version}
        if "dials" in out:
            out["dials"]["requests"] = self.variant_requests
        return out

    def request_variants(self, slides: list[str], a: int, d: int, current: str | None = None) -> list[str]:
        """Queue a rewrite of each slide at a setting it has not been written at; return the ones queued."""
        setting = _setting(self.deck.spec, [])
        if setting is None:
            raise ValueError("this deck has no audience and detail setting")
        key, fresh = f"{a}-{d}", self.deck.extra["variants"].fresh
        pending = {(x["slide"], x["setting"]) for x in self.queue.items() if x.get("kind") == "variant"}
        queued = []
        for sid in dict.fromkeys(slides):
            s = self.deck.slide(sid)
            if s is None or not variant_dials(s):
                raise ValueError(f"{sid}: not a slide that takes another setting")
        for sid in dict.fromkeys(slides):
            s = self.deck.slide(sid)
            if (a, d) == setting or key in fresh.get(sid, {}) or (sid, key) in pending:
                continue
            self.queue.add({"id": new_id(), "kind": "variant", "slide": sid, "setting": key, "audience": a, "detail": d,
                            "current": sid == current,
                            "where": f"{self.deck.position(s)}: {s['title']}", "at": time.time()})
            queued.append(sid)
        self._sync_variant_requests(self.queue.items())
        return queued

    def _sync_variant_requests(self, items: list[dict]):
        """Drop requests that a stored variant answers or that can no longer be stored, and tell the
        pages what is queued or being written."""
        fresh = self.deck.extra["variants"].fresh
        setting = _setting(self.deck.spec, [])
        now: dict[str, dict[str, str]] = {}
        for x in items:
            if x.get("kind") != "variant":
                continue
            s = self.deck.slide(x["slide"])
            if (x["setting"] in fresh.get(x["slide"], {}) or s is None or not variant_dials(s) or setting is None
                    or x["setting"] == f"{setting[0]}-{setting[1]}"):
                self.queue.remove(x["id"])
                continue
            now.setdefault(x["slide"], {})[x["setting"]] = "claimed" if self.queue.claimed(x["id"]) else "queued"
        if now != self.variant_requests:
            self.variant_requests = now
            self.broadcast("variants", {"requests": now})

    def hunks(self) -> dict[str, Hunk]:
        """The hunks a reader can quote: the deck's and any an extension adds."""
        out = dict(self.deck.hunks)
        for ext in EXTENSIONS:
            out.update(ext.hunks(self))
        return out

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
            working = False
            try:
                self._sync_queue()
                for ext in EXTENSIONS:
                    ext.tick(self)
                working = any(ext.busy(self) for ext in EXTENSIONS)
            except Exception as e:  # keep watching; the log says what failed
                log(f"queue: {e!r}")
            now = time.time()
            if self.queue.listener():
                self.last_listener = now
            if now - self.last_listener < LISTEN_GRACE:
                presence = "listening"
            else:
                presence = "answering" if self.busy else "working" if working else "away"
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
            old, self.deck, self.deck_errors = self.deck, deck, []
            self.version += 1
            self.issues.clear()
        self.mtimes = self._mtimes()
        for ext in EXTENSIONS:
            try:
                ext.reloaded(self, old)
            except Exception as e:  # the new deck stands; the log says what failed
                log(f"{ext.name}: {e!r}")
        self.broadcast("deck", {"version": self.version})
        log(f"deck reloaded (version {self.version})")

    def _sync_queue(self):
        changed = set()
        busy = False
        items = self.queue.items()
        for item in items:
            qid = item["id"]
            if item.get("kind") == "variant":
                busy |= self.queue.claim_age(qid) < CLAIM_STALE
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
        self._sync_variant_requests(items)
        for slide in changed:
            self.broadcast("thread", {"slide": slide})

    def _sync_answer(self, item: dict):
        """Mark a written answer as being judged, or record the session's verdict."""
        qid, slide, question = item["id"], item["slide"], item["question"]
        key = item.get("qkey", slide)
        if self.queue.answered(qid):
            verdict = self.queue.file(qid, ".verdict")
            verdict = verdict.read_text().strip() if verdict.exists() else ""
            feedback = self.queue.file(qid, ".answer.md").read_text()
            verdict = verdict if verdict in VERDICTS else None
            self.queue.remove(qid)
            changed = self.quiz.update(key, question, qid, state="judged", verdict=verdict, feedback=feedback)
        elif self.queue.claimed(qid):
            # A deck edit can leave a choice index where the written answer was.
            a = self.quiz.get(key, question)
            changed = isinstance(a, dict) and a.get("state") == "queued" and self.quiz.update(key, question, qid, state="judging")
        else:
            changed = False
        if changed:
            self.broadcast("quiz", {"slide": slide})


def log(msg: str):
    print(time.strftime("%H:%M:%S"), msg, file=sys.stderr, flush=True)


class ClientGone(Exception):
    pass


class Handler(BaseHTTPRequestHandler):
    server_version = "walkthrough"
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
            "/api/variant": self._variant,
            "/api/variant-withdraw": self._variant_withdraw,
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
        hunks = st.hunks()
        refs = parse_refs(hunks, body.get("refs"))
        if slide is None or not isinstance(text, str) or not (text.strip() or refs):
            return self._error(400, "expected a slide id and a question")
        qid = new_id()
        view = body.get("view")
        view = view if isinstance(view, str) and view in deck.extra["variants"].fresh.get(slide["id"], {}) else None
        msg = {"id": qid, "role": "user", "text": text.strip()[:20_000], "refs": refs,
               "at": time.time(), "state": "queued", **({"view": view} if view else {})}
        item = {
            "id": qid,
            "kind": "question",
            "view": view,
            "slide": slide["id"],
            "where": f"{deck.position(slide)}: {slide['title']}",
            "text": msg["text"],
            "refs": refs_text(hunks, refs),
            "at": msg["at"],
        }
        with st.threads.lock:
            st.threads.messages(slide["id"]).append(msg)
            st.threads.save()
            st.queue.add(item)
        st.broadcast("thread", {"slide": slide["id"]})
        self._json({"message": msg})

    def _variant(self, body: dict):
        """The reader asked for slides at another setting."""
        slides = body.get("slides")
        try:
            a, d = parse_stop(body.get("audience"), "audience"), parse_stop(body.get("detail"), "detail")
            if not isinstance(slides, list) or not slides or not all(isinstance(x, str) for x in slides):
                raise ValueError("expected a list of slide ids")
            current = body.get("current") if isinstance(body.get("current"), str) else None
            queued = self.state.request_variants(slides, a, d, current)
        except ValueError as e:
            return self._error(400, str(e))
        self._json({"queued": queued, "requests": self.state.variant_requests})

    def _variant_withdraw(self, body: dict):
        """The reader no longer wants a rewrite that the session has not started."""
        st = self.state
        slides, setting = body.get("slides"), body.get("setting")
        if not isinstance(slides, list) or not isinstance(setting, str):
            return self._error(400, "expected slide ids and a setting")
        removed = []
        for x in st.queue.items():
            if x.get("kind") == "variant" and x["slide"] in slides and x["setting"] == setting and not st.queue.claimed(x["id"]):
                st.queue.remove(x["id"])
                removed.append(x["slide"])
        st._sync_variant_requests(st.queue.items())
        self._json({"withdrawn": removed, "requests": st.variant_requests})

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
        base = st.deck.slide(str(body.get("slide")))
        versions = check_versions(st.deck, base["id"]) if base and base["quiz"] else []
        # Without a version, the check as written: what a page from before versions sends.
        slide = next((v for v in versions if v["qkey"] == body.get("version", base and base.get("qkey"))), None)
        if versions and slide is None:
            return self._error(409, "this version of the check has changed; reload the page")
        question, choice, text = body.get("question"), body.get("choice"), body.get("text")
        q = slide["quiz"][question] if slide and isinstance(question, int) and 0 <= question < len(slide["quiz"]) else None
        if q is None:
            return self._error(400, "expected a check slide and a question index")
        sid, key = slide["id"], slide["qkey"]
        if q["kind"] == "choice":
            if not (choice is None or isinstance(choice, int) and 0 <= choice < len(q["choices"])):
                return self._error(400, "expected a choice index")
            st.quiz.set(key, question, choice)
        else:
            prev = st.quiz.get(key, question)
            if isinstance(prev, dict) and prev.get("state") in ("queued", "judging"):
                if st.queue.claim_age(prev["id"]) < CLAIM_STALE:
                    return self._error(409, "the session is judging this answer")
                st.queue.remove(prev["id"])
            if text is None:
                st.quiz.set(key, question, None)
            elif not isinstance(text, str) or not text.strip():
                return self._error(400, "expected the answer's text")
            else:
                qid, now = new_id(), time.time()
                st.quiz.set(key, question, {"id": qid, "text": text[:50_000], "state": "queued", "at": now})
                st.queue.add({"id": qid, "kind": "answer", "slide": sid, "qkey": key, "question": question, "at": now,
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
    if deck.spec.get("reader"):
        print(f"reader: {deck.spec['reader']}")
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


# --------------------------------------------------------------------------
# The deck as text
#
# `text` prints what the page shows, in slide order, for a reader that has
# no browser: a cold reader that looks for what the deck leaves undefined.
# It leaves out the answers to the checks unless asked for the key.

COLD_READ = """\
You are a test reader for the slide deck below. Its author knows the subject too well to see what the deck
does not explain. Your report shows the author where a new reader stops understanding.

## The reader you stand in for

{reader}
{setting}
- Do not report what this reader knows.
- Read as a person who does not know the subjects this reader is new to. You possibly know a term from those
  subjects yourself. If the deck uses the term and does not explain it, report it.
- The reader sees only the deck. The reader did not see its sources, or the work and the discussions behind it.

## Rules

- Use only the text below. Do not open files, search, or run commands.
- Read the slides in order, one time. Record a problem at the slide where you find it, before you read on.
- Answer each check when you reach it, from the slides before it.
- Do not review the subject. Do not report defects in the code or errors in the sources.
- Do not rewrite slides or suggest wording. Do not report typing errors, style or layout.

## Your report

Write Markdown with these four sections and nothing else.

### Summaries

One sentence for each content slide, by slide id: the idea the slide teaches, in your own words. Write
"unclear" if you cannot say it.

### Gaps

At most 12, the most serious first. Give each gap these fields:

- **Slide**: the slide id.
- **Kind**: one of these.
  - `term`: the deck uses a name, abbreviation or identifier before it says what it is.
  - `leap`: a claim or step does not follow from the slides before it.
  - `order`: the deck explains it, but later than the reader needs it.
  - `why`: the deck says what was done or decided, and not the reason, which the goal needs.
  - `question`: the goal makes the reader ask another question, which the deck does not answer.
- **Quote**: the words on the slide, 15 words at most.
- **Question**: what you needed to know, as the question the reader would ask.
- **Effect**: `blocks` (you could not follow the slide) or `slows` (you continued on a guess, and you give the guess).

### Checks

For each check question, by its id:

- **Answer**: the number of the choice, or your written answer.
- **Basis**: the ids of the slides that gave you the answer. Write `guess` if you did not know. Write
  `background` if you knew the answer without the deck. Write `wording` if you chose the answer because it
  repeats a slide's words or because the other choices are implausible.
- **Confidence**: `high` or `low`.

### Goal

Two sentences at most: can you now do what the goal says, and if not, what is missing most.

---

"""

BLIND_CHECKS = """\
Below are the check questions of a slide deck, without the deck. Answer each question from general knowledge
and from the wording of the question and its choices. Do not open files, search, or run commands. The author
uses your answers to find questions that a reader can answer without reading the deck.

For each question, by its id, give the number of the choice or a written answer in one or two sentences, and
your confidence: `high` or `low`. Write nothing else.

---

"""

TAG_RE = re.compile(r"<[^>]+>")
SVG_TEXT_RE = re.compile(r"<text\b[^>]*>(.*?)</text>", re.S)
ASK_RE = re.compile(r"""\bdata-ask=(?:"([^"]*)"|'([^']*)')""")


def _labels(markup: str, texts: re.Pattern | None = None) -> list[str]:
    """The words a reader sees in SVG or HTML markup, and the parts they can click."""
    shown = texts.findall(markup) if texts else [re.sub(r"<(script|style)\b.*?</\1>", " ", markup, flags=re.S)]
    words = [" ".join(html.unescape(TAG_RE.sub(" ", t)).split()) for t in shown]
    asks = [html.unescape(a or b) for a, b in ASK_RE.findall(markup)]
    return list(dict.fromkeys(w for w in words + asks if w))


def diagram_text(d: dict) -> str:
    out = [f"[Diagram ({d['type']})]" + (f" {d['caption']}" if d.get("caption") else "")]
    if d.get("alt"):
        out.append(f"Description: {d['alt']}")
    if d["type"] == "mermaid":
        out.append(fenced(d["source"], "mermaid"))
    elif d["type"] == "svg":
        out.append("Labels: " + " | ".join(_labels(d["svg"], SVG_TEXT_RE)))
    elif d["type"] == "html":
        out.append("Text: " + " | ".join(_labels(d["html"])))
    else:
        for n, fr in enumerate(d["frames"], 1):
            out.append(f"Frame {n}" + (f": {fr['caption']}" if fr["caption"] else ""))
            out.append(fenced(fr["mermaid"], "mermaid") if fr["mermaid"] else "Labels: " + " | ".join(_labels(fr["svg"], SVG_TEXT_RE)))
    return "\n".join(out)


def block_text(deck: Deck, blk: dict) -> str:
    kind = blk["type"]
    if kind == "md":
        return blk["md"]
    if kind == "callout":
        return f"[Callout: {blk['tone']}] {blk['md']}"
    if kind == "diagram":
        return diagram_text(blk["diagram"])
    if kind == "excerpt":
        out = [blk["caption"]] if blk.get("caption") else []
        for p in blk["pieces"]:
            h = deck.hunks[p["hunk"]]
            lines = numbered(h, p["lo"], p["hi"])
            head = f"[Excerpt: {h.file.path}, lines {span(h, p['lo'], p['hi'])}]" if h.file.plain else "[Diff excerpt]"
            out.append(f"{head}\n{fenced(lines, h.file.language if h.file.plain else 'diff')}")
        if not blk["pieces"]:
            f = next(f for f in deck.files if f.key == blk["file"])
            out.append(f"[File: {f.path}, {f.describe()}]")
        return "\n".join(out)
    if kind == "group":
        return "\n\n".join([f"### {blk['title']}\n\n{blk['md']}", *(block_text(deck, b) for b in blk["blocks"])])
    return ""


def quiz_text(s: dict, key: bool) -> str:
    out = []
    for n, q in enumerate(s["quiz"], 1):
        qid = f"{s['id']}.{n}"
        if q["kind"] == "choice":
            out.append(f"**{qid}** (choose one) {q['q']}\n\n" + "\n".join(f"{j}. {c['text']}" for j, c in enumerate(q["choices"], 1)))
            if key:
                j = next(j for j, c in enumerate(q["choices"], 1) if c["correct"])
                out.append(f"Key: {j}. Settled by slide `{q['see']}`.")
        else:
            starter = f"\n\n{fenced(q['starter'], q.get('language', ''))}" if q.get("starter") else ""
            out.append(f"**{qid}** (written answer, {q['kind']}) {q['q']}{starter}")
            if key:
                out.append(f"Rubric: {q['rubric']} Settled by slide `{q['see']}`.")
    return "\n\n".join(out)


def at_setting(deck: Deck, a: int, d: int) -> tuple[list[dict], list[str]]:
    """The slides with each one that has a fresh variant at this setting replaced by it, and the ids still as written."""
    fresh = deck.extra["variants"].fresh
    slides, missing = [], []
    for s in deck.slides:
        v = fresh.get(s["id"], {}).get(f"{a}-{d}")
        if v is None and variant_dials(s) and (a, d) != _setting(deck.spec, []):
            missing.append(s["id"])
        slides.append(v or s)
    return slides, missing


def deck_text(deck: Deck, appendix: bool = False, key: bool = False, checks_only: bool = False,
              slides: list[dict] | None = None) -> str:
    spec = deck.spec
    out = [f"# {spec['title']}"]
    if not checks_only:
        out += [f"{label}{spec[k]}" for k, label in (("subtitle", ""), ("scope", "Sources: "), ("goal", "Goal: "), ("reader", "Reader: "))
                if spec.get(k)]
    slides = [s for s in (slides or deck.slides) if not s.get("generated")]
    shown = [s for s in slides if appendix or s["section"] != "Appendix"]
    skipped = [s["title"] for s in slides if s not in shown]
    for n, s in enumerate(shown, 1):
        if checks_only and not s["quiz"]:
            continue
        section = f" ({s['section']})" if s["section"] else ""
        head = f"## Slide {n} of {len(shown)} `{s['id']}`{section}: {s['title']}"
        body = quiz_text(s, key) if s["quiz"] else "\n\n".join(filter(None, (block_text(deck, b) for b in s["blocks"])))
        out.append(f"{head}\n\n{body}")
    if skipped and not checks_only:
        out.append("## Appendix (not shown)\n\n" + "\n".join(f"- {t}" for t in skipped))
    return "\n\n".join(out) + "\n"


def cmd_text(args):
    deck = load_or_exit(Path(args.deck))
    brief = ""
    if args.brief and args.key:
        sys.exit("error: --brief is for a reader who must not see the key")
    if args.brief and args.checks_only:
        brief = BLIND_CHECKS
    elif args.brief:
        if not deck.spec.get("reader"):
            sys.exit("error: reader: say in deck.json what the reader knows and is new to, so the cold reader can stand in for them")
    base = _setting(deck.spec, [])
    a, d = base or (None, None)
    if args.brief and (args.audience or args.detail):
        sys.exit("error: --brief is for the deck as written; a slide rewritten at another setting is not cold-read")
    if args.audience or args.detail:
        if base is None:
            sys.exit("error: the deck has no audience and detail setting")
        try:
            a = parse_stop(args.audience or base[0], "audience")
            d = parse_stop(args.detail or base[1], "detail")
        except ValueError as e:
            sys.exit(f"error: {e}")
    if args.brief and not args.checks_only:
        brief = COLD_READ.format(reader=deck.spec["reader"], setting=setting_brief(a, d) if base else "")
    slides = None
    if args.audience or args.detail:
        slides, missing = at_setting(deck, a, d)
        if missing:
            print(f"note: not written at {setting_text(a, d)}, shown as written: {', '.join(missing)}", file=sys.stderr)
    print(brief + deck_text(deck, args.appendix, args.key, args.checks_only, slides), end="")


def setting_brief(a: int, d: int) -> str:
    """For the cold reader: the setting the deck is written at, so that it does not report what the setting leaves out."""
    return (f"\nThe deck is written at a set level of detail, {stop_definition('detail', d)}\n\n"
            "Report a gap that this level of detail leaves out by design only when the slide cannot be followed "
            "without it. The statement above, not the level of detail, says what the reader knows.\n")


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
        return f"{ENTRY.name} or its page changed since it started"
    return f"it runs {info.get('script', 'an older deck.py')}"


def print_address(info: dict):
    print(info["url"])
    if info.get("tunnel"):
        print(f"From another machine, forward the port first: {info['tunnel']}")


def stop_server(deck_dir: Path, info: dict):
    try:
        # A server runs the script that started it, this one or an extension's.
        script = Path(info.get("script") or "deck.py").name
        if not script.endswith(".py") or script.encode() not in Path(f"/proc/{info['pid']}/cmdline").read_bytes():
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
        "extensions": [e.name for e in EXTENSIONS],
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
    # A server from before this field was recorded is out of date anyway, which the check below reports.
    if info.get("extensions", [e.name for e in EXTENSIONS]) != [e.name for e in EXTENSIONS]:
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
            # A rewrite request has no answer file; once claimed, it waits for its variant, unless the claim is stale.
            pending = [x for x in q.items() if x["id"] != args.answer and not q.answered(x["id"])
                       and not (x.get("kind") == "variant" and q.claim_age(x["id"]) < CLAIM_STALE)
                       and not any(ext.skip(q, x) for ext in EXTENSIONS)]
            # The rewrite of the slide the reader is on first, then questions, then the rest of a batch of rewrites.
            pending.sort(key=lambda x: (not q.claimed(x["id"]), 0 if x.get("current") else 2 if x.get("kind") == "variant" else 1,
                                        x.get("at", 0)))
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
    if item.get("kind") == "variant":
        batch = [x for x in pending if x.get("kind") == "variant" and (x.get("current") or not item.get("current"))]
        return listen_variants(deck_dir, q, batch, ctx)
    if any(ext.listen(item, ctx) for ext in EXTENSIONS):
        return
    if item.get("kind") == "finished":
        q.remove(item["id"])
        print("The reader marked the walkthrough finished.")
        try:
            saved = QuizAnswers(deck_dir / "quiz.json")
            report = quiz_report(load_deck(deck_dir), saved.snapshot(), saved.at)
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
    setting = _setting(read_json(deck_dir / "deck.json"), [])
    if item.get("view"):
        a, d = map(int, item["view"].split("-"))
        print(f"The reader sees this slide at {setting_text(a, d)}, not as written: "
              f"{deck_dir / 'variants' / item['slide'] / (item['view'] + '.json')}\n")
    elif setting:
        a, d = setting
    if setting:
        print(f"Answer for {setting_text(a, d)}, unless the question asks for another level: define the terms that "
              f"audience needs, and treat the detail stop as an upper bound on length, never a target.")
        if (a, d) != setting:
            print(f"  audience {stop_definition('audience', a)}")
            print(f"  To change this slide as the reader sees it, store it again with `variant`; a factual "
                  f"correction also goes into deck.json, which retires the rewrites of that slide.")
        print()
    if item.get("refs"):
        print(item["refs"] + "\n")
    print(item["text"] or "(No text; the reader wants the selection explained.)")
    extras = [x for ext in EXTENSIONS if (x := ext.question(ctx))]
    for extra in extras:
        print("\n" + extra)
    print(
        "\nAnswer for the slide's chat panel: concise Markdown, citing path:line. Read what you"
        f" need; change the deck only if the reader asks for that{', or as above' if extras else ''}. Write the answer to a file,"
        " then post it and keep listening:\n"
        f"  python3 {me} listen {deck_arg} --answer {item['id']} --file ANSWER.md{wait}"
    )
    if rest:
        print(f"({rest} more waiting.)")


def listen_variants(deck_dir: Path, q: Queue, items: list[dict], ctx: Listening):
    """Hand the session every waiting request for a slide at another setting, at once."""
    try:
        deck = load_deck(deck_dir)
    except DeckInvalid as e:
        print("The reader asked for slides at another setting, but deck.json does not check, so they cannot be "
              f"stored yet. Fix these, then listen again:\n  " + "\n  ".join(e.errors) + f"\n  {ctx.again}")
        return
    setting = _setting(deck.spec, [])
    if setting is None:
        for x in items:
            q.remove(x["id"])
        print(f"Dropped {plural(len(items), 'request')} for another setting: the deck no longer has one.\n  {ctx.again}")
        return
    a0, d0 = setting
    for x in items:
        write_atomic(q.file(x["id"], ".claimed"), json.dumps({"pid": os.getpid(), "at": time.time()}))
    settings = sorted({(x["audience"], x["detail"]) for x in items})
    print(f"The reader asked for {plural(len(items), 'slide')} at another setting. The deck is written at "
          f"{setting_text(a0, d0)}.\n")
    for x in items:
        print(f"  - {x['slide']} ({x['where']}) at {setting_text(x['audience'], x['detail'])}")
    print("\nThe stops asked for (references/dials.md has all of them):")
    for dial, n in sorted({("audience", a) for a, _ in settings} | {("detail", d) for _, d in settings}):
        print(f"  {dial} {n}: {stop_definition(dial, n)}")
    rules = "\"Rewriting a slide\"" + (" and \"Rewriting a slide of a change\"" if "coverage" in deck.extra else "")
    print(f"\nRewrite each slide in place as {rules} in references/dials.md say: the same idea and id, one slide, "
          f"no new slides, every check question that relies on a content slide still settled, and a check's "
          f"questions written for the slides it covers at that setting. This prints the full brief "
          f"for one slide (the rules, the deck's outline, the slide as written and the checks on it); give it as is to "
          f"a subagent, or follow it yourself:\n"
          f"  python3 {ctx.me} variant {ctx.deck_arg} SLIDE --audience A --detail D --brief\n"
          f"Each rewrite is stored with `variant ... --file`, and the page shows it at once. With several slides, "
          f"rewrite them in parallel if your harness has subagents. `listen` does not hand these requests out again "
          f"unless they wait for {CLAIM_STALE // 60} minutes. Then keep listening with:\n  {ctx.again}")


def dials_section(title: str) -> str:
    """A `## ` or `### ` section of references/dials.md, without its heading."""
    try:
        doc = DIALS_DOC.read_text()
    except OSError:
        return ""
    m = re.search(rf"^(#+) {re.escape(title)}\n(.*?)(?=^#{{1,3}} |\Z)", doc, re.M | re.S)
    return m[2].strip() if m else ""


def variant_brief(deck: Deck, sid: str, a: int, d: int) -> str:
    """Everything a fresh agent needs to rewrite one slide at a setting: the rules, the deck around it,
    the slide as written, the checks that rely on it, and how to store the result."""
    spec = deck.spec
    a0, d0 = _setting(spec, [])
    raw = next(x for x in spec["slides"] if x.get("id") == sid)
    base = deck.slide(sid)
    if base["quiz"]:
        # A check tests what its slides teach at this setting: give them as the reader sees them there.
        fresh = deck.extra["variants"].fresh
        covered = []
        for x in check_sees(base):
            v = fresh.get(x, {}).get(f"{a}-{d}")
            shown = v or deck.slide(x)
            body = "\n\n".join(filter(None, (block_text(deck, b) for b in shown["blocks"])))
            covered.append(f"### `{x}`: {shown['title']} ({'rewritten at this setting' if v else 'as written'})\n\n{body}")
        checks = []
    else:
        checks = [f"- {c['id']}: {q['q']}" for c in deck.checks() for q in c["quiz"]
                  if q["see"] == sid or any(ch["see"] == sid for ch in q.get("choices", []))]
    outline = "\n".join(f"- {x['id']}: {x['title']}" for x in deck.slides if not x.get("generated"))
    change = dials_section("Rewriting a slide of a change") if "coverage" in deck.extra else ""
    parts = [
        f"Rewrite one slide of the walkthrough deck below at {setting_text(a, d)}. The deck is written at "
        f"{setting_text(a0, d0)}.",
        f"## The stops\n\n- Audience {stop_definition('audience', a)}\n- Detail {stop_definition('detail', d)}",
        # The section's first paragraph tells the session how to get this brief; the rest are the rules.
        f"## Rewriting a slide\n\n{dials_section('Rewriting a slide').split(chr(10) * 2, 1)[-1]}",
        f"## Rewriting a check\n\n{dials_section('Rewriting a check')}" if base["quiz"] else "",
        f"## Rewriting a slide of a change\n\n{change}" if change and not base["quiz"] else "",
        "## The deck\n\n" + "\n".join(f"{k.title()}: {spec[k]}" for k in ("title", "goal", "reader") if spec.get(k))
        + f"\n\nSlides in order:\n{outline}",
        "## Checks that rely on this slide\n\nThe rewrite must still settle each of these:\n\n" + "\n".join(checks) if checks else "",
        "## The slides this check covers\n\nWrite the questions from these, as the reader sees them at this setting:\n\n"
        + "\n\n".join(covered) if base["quiz"] else "",
        f"## The slide as written\n\n{fenced(json.dumps(raw, indent=1, ensure_ascii=False), 'json')}",
        "## Sources and storing\n\n"
        + (f"Quoted paths resolve against {(deck.dir / spec['root']).resolve()}. " if spec.get("root") else "")
        + f"The deck directory is {deck.dir}. The block format is in {SCRIPT.parent.parent / 'references' / 'deck.md'}"
        + (" and walkthrough-pr's references/diffs.md; `hunks DECK` lists the diff's hunks." if "coverage" in deck.extra else ".")
        + f" Change no file but the one you write. Write the slide object (title, prompts, {'quiz' if base['quiz'] else 'blocks'}) to a file, then store it, "
        f"fixing what it reports until it stores:\n\n  python3 {shlex_path(ENTRY)} variant {shlex_path(deck.dir)} {sid} "
        f"--audience {a} --detail {d} --file FILE",
    ]
    return "\n\n".join(p for p in parts if p) + "\n"


def plural(n: int, one: str) -> str:
    return f"{n} {one}" if n == 1 else f"{n} {one}s"


def cmd_variant(args):
    """Store, show, list or prune variants."""
    deck_dir = Path(args.deck).resolve()
    deck = load_or_exit(deck_dir)
    setting = _setting(deck.spec, [])
    variants: Variants = deck.extra["variants"]
    vdir = deck_dir / "variants"
    if setting is None:
        sys.exit("error: the deck has no setting; give deck.json `audience` and `detail` (see references/dials.md)")
    if args.prune:
        for p in variants.stale + variants.invalid:
            p.unlink(missing_ok=True)
        for d in vdir.iterdir() if vdir.is_dir() else ():
            if d.is_dir() and not any(d.iterdir()):
                d.rmdir()
        print(f"Removed {plural(len(variants.stale), 'stale variant')} and {plural(len(variants.invalid), 'invalid file')}.")
        return
    if not args.slide:
        dials = [s for s in deck.slides if variant_dials(s)]
        print(f"Written at {setting_text(*setting)}. Variants by slide (A-D; * stale):")
        stale = defaultdict(list)
        for p in variants.stale:
            stale[p.parent.name].append(p.stem + "*")
        size = sum(p.stat().st_size for p in vdir.rglob("*.json")) if vdir.is_dir() else 0
        for s in dials:
            have = sorted(variants.fresh.get(s["id"], {})) + stale[s["id"]]
            print(f"  {s['id']}: {' '.join(have) or '-'}")
        n = sum(len(v) for v in variants.fresh.values())
        print(f"{n} fresh, {len(variants.stale)} stale, {len(variants.invalid)} invalid, {size / 1024:.0f} KiB on disk")
        for e in variants.errors:
            print(f"error: {e}")
        return
    base = deck.slide(args.slide)
    if base is None or not variant_dials(base):
        sys.exit(f"error: {args.slide}: not a slide that takes another setting (generated slides keep one)")
    raw_base = next(s for s in deck.spec["slides"] if s.get("id") == args.slide)
    try:
        a = parse_stop(args.audience, "audience") if args.audience is not None else None
        d = parse_stop(args.detail, "detail") if args.detail is not None else None
    except ValueError as e:
        sys.exit(f"error: {e}")
    path = vdir / args.slide / f"{a}-{d}.json"
    if args.brief:
        if a is None or d is None or (a, d) == setting:
            sys.exit("error: --brief takes --audience and --detail other than the deck's own setting")
        print(variant_brief(deck, args.slide, a, d), end="")
        return
    if args.show:
        if a is None and d is None or (a, d) == setting:
            print(json.dumps(raw_base, indent=1))
        elif a is None or d is None:
            sys.exit("error: give both --audience and --detail, or neither for the slide as written")
        elif f"{a}-{d}" in variants.fresh.get(args.slide, {}):
            print(json.dumps(json.loads(path.read_text())["slide"], indent=1))
        else:
            sys.exit(f"error: {args.slide} has no fresh variant at {setting_text(a, d)}")
        return
    if a is None or d is None or not args.file:
        sys.exit("error: storing a variant takes --audience, --detail and --file")
    if (a, d) == setting:
        sys.exit("error: that is the deck's own setting; edit the slide in deck.json instead")
    try:
        raw = json.loads(Path(args.file).read_text())
    except (OSError, ValueError) as e:
        sys.exit(f"error: {args.file}: {e}")
    body = "quiz" if base["quiz"] else "blocks"
    if not isinstance(raw, dict) or raw.get("id", args.slide) != args.slide or body not in raw:
        sys.exit(f"error: {args.file}: expected one slide object with id {args.slide!r} (or none) and {body}")
    raw = {k: v for k, v in raw.items() if k in ("title", "prompts", body)}
    rec = {"key": variants.keys[args.slide], "slide": raw, "at": time.time()}
    for k in ("seconds", "tokens"):
        if getattr(args, k) is not None:
            rec[k] = getattr(args, k)
    path.parent.mkdir(parents=True, exist_ok=True)
    old = path.read_text() if path.exists() else None
    write_atomic(path, json.dumps(rec, indent=1))
    deck = load_or_exit(deck_dir)
    stored: Variants = deck.extra["variants"]
    errors = [e for e in stored.errors if f"variants/{args.slide}/{a}-{d}.json" in e]
    if not errors:
        rec["lines"] = lines_key(stored.fresh[args.slide][f"{a}-{d}"], deck.hunks)
        write_atomic(path, json.dumps(rec, indent=1))
    if errors:
        if old is None:
            path.unlink()
        else:
            write_atomic(path, old)
        for e in errors:
            print(f"error: {e}")
        sys.exit(1)
    q = Queue(deck_dir)
    for x in q.items():
        if x.get("kind") == "variant" and x["slide"] == args.slide and x["setting"] == f"{a}-{d}":
            q.remove(x["id"])
    print(f"Stored {args.slide} at {setting_text(a, d)}. An open page shows it now.")


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

    x = sub.add_parser(
        "text",
        help="print the deck as the reader sees it, in slide order",
        description="Print the deck as Markdown: each slide's prose, callouts, excerpts and diagram labels, and "
        "the check questions without their answers. Give this to a cold reader.",
    )
    x.add_argument("deck")
    x.add_argument("--appendix", action="store_true", help="also print the appendix slides (default: their titles)")
    x.add_argument("--key", action="store_true", help="also print each check question's answer, for grading")
    x.add_argument("--checks-only", action="store_true", help="print the check slides alone")
    x.add_argument("--audience", "-a", help="print each slide at this audience stop where it has been written at it")
    x.add_argument("--detail", "-d", help="print each slide at this detail stop where it has been written at it")
    x.add_argument("--brief", action="store_true", help="start with the instructions for a cold reader "
                   "(with --checks-only: for a reader who answers the checks without the deck)")
    x.set_defaults(func=cmd_text)

    v = sub.add_parser(
        "variant",
        help="store, show or list slides rewritten at another audience and detail setting",
        description="With SLIDE, --audience, --detail and --file: store a slide rewritten at that setting. "
        "With --show: print the slide as written, or at a setting. Without SLIDE: list the variants. "
        "Stops are 1-5 or their names; see references/dials.md.",
    )
    v.add_argument("deck")
    v.add_argument("slide", nargs="?")
    v.add_argument("--audience", "-a", help="1-5 or " + "|".join(AUDIENCE))
    v.add_argument("--detail", "-d", help="1-5 or " + "|".join(DETAIL))
    v.add_argument("--file", help="the rewritten slide, as a deck.json slide object")
    v.add_argument("--show", action="store_true", help="print the slide's JSON instead of storing one")
    v.add_argument("--brief", action="store_true", help="print the brief for an agent that rewrites the slide at a setting")
    v.add_argument("--prune", action="store_true", help="delete stale variants and variant files that do not load")
    v.add_argument("--seconds", type=float, help=argparse.SUPPRESS)
    v.add_argument("--tokens", type=int, help=argparse.SUPPRESS)
    v.set_defaults(func=cmd_variant)

    for ext in EXTENSIONS:
        ext.commands(sub)

    t = sub.add_parser("stop", help="stop a detached server")
    t.add_argument("deck")
    t.set_defaults(func=cmd_stop)

    args = p.parse_args(argv)
    args.func(args)


if __name__ == "__main__":
    main()
