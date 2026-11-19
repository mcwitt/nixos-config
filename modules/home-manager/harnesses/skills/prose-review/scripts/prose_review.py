#!/usr/bin/env python3
"""Serve a Markdown document for review beside a chat panel.

`check` validates a review directory and prints the document's outline.
`serve` publishes the rendered document as a local web page with a chat
panel, where the reader selects passages to ask about or to have changed.
`listen` hands the reader's questions to the review session and posts its
answers back; `stop` ends a detached server.

A review directory holds `review.json`, which names the Markdown file under
review; the file itself stays where it is. The server writes `threads.json`
(the chat history), `server.json`, `server.log` and a `queue/` directory next
to it, and re-reads the document whenever it changes, so edits made in the
session appear in the open page and are marked as changed. Questions and
answers cross between the server and `listen` only as files in `queue/`, so
`listen` needs no network access. The server side and the page chrome
(ui.js, ui.css) are shared with walkthrough, in lib/. Requires Python >= 3.11;
uses only the standard library.
"""

from __future__ import annotations

import argparse
import functools
import json
import re
import sys
import time
from dataclasses import dataclass
from pathlib import Path

ASSETS = Path(__file__).resolve().parent.parent / "assets"
SCRIPT = Path(__file__).resolve()
# The server side shared with walkthrough: this skill's lib, a symlink to it in the repository.
LIB = SCRIPT.parent.parent / "lib"
if str(LIB) not in sys.path:
    sys.path.insert(0, str(LIB))
import session  # noqa: E402
from session import LIB_ASSETS, QUESTION_ID, Queue, log, new_id, shlex_path, write_atomic  # noqa: E402

HEADING_RE = re.compile(r"^ {0,3}(#{1,6})\s+(.*?)\s*#*\s*$")
FENCE_RE = re.compile(r"^ {0,3}(`{3,}|~{3,})")
THREAD = "doc"  # one conversation per review
VERSIONS_KEPT = 50


@functools.cache
def asset_files() -> dict[str, Path]:
    """The page's files by name: this skill's and the shared chrome."""
    return session.page_files([ASSETS, LIB_ASSETS])


@functools.cache
def engine() -> str:
    """A digest of the scripts and page assets: a deploy changes it."""
    return session.digest([SCRIPT, *asset_files().values()])


# --------------------------------------------------------------------------
# The document


class ReviewInvalid(Exception):
    def __init__(self, errors: list[str]):
        super().__init__("\n".join(errors))
        self.errors = errors


@dataclass
class Review:
    dir: Path
    spec: dict
    file: Path
    text: str
    title: str

    @property
    def lines(self) -> list[str]:
        """Source lines as the page numbers them; a final newline adds no line."""
        out = self.text.split("\n")
        if out and out[-1] == "":
            out.pop()
        return out

    @property
    def sources(self) -> list[Path]:
        return [self.dir / "review.json", self.file]


def outline(text: str) -> list[tuple[int, int, str]]:
    """(line, level, title) for each ATX heading outside fenced code."""
    out = []
    fence = None
    for i, line in enumerate(text.split("\n"), 1):
        m = FENCE_RE.match(line)
        if m:
            if fence is None:
                fence = m.group(1)
            elif line.strip().startswith(fence[0] * len(fence)):
                fence = None
            continue
        if fence:
            continue
        m = HEADING_RE.match(line)
        if m:
            out.append((i, len(m.group(1)), m.group(2)))
    return out


def _str(v, where: str, errors: list[str], required: bool = False) -> str:
    if v is None and not required:
        return ""
    if not isinstance(v, str) or (required and not v.strip()):
        errors.append(f"{where}: expected a non-empty string")
        return ""
    return v


def load_review(review_dir: Path) -> Review:
    review_dir = review_dir.resolve()
    spec_path = review_dir / "review.json"
    try:
        spec = json.loads(spec_path.read_text())
    except FileNotFoundError:
        raise ReviewInvalid([f"{spec_path}: not found"])
    except json.JSONDecodeError as e:
        raise ReviewInvalid([f"{spec_path}: invalid JSON: {e}"])
    if not isinstance(spec, dict):
        raise ReviewInvalid([f"{spec_path}: expected a JSON object"])
    errors: list[str] = []
    name = _str(spec.get("file"), "file", errors, required=True)
    for key in ("title", "subtitle", "source_url"):
        _str(spec.get(key), key, errors)
    if errors:
        raise ReviewInvalid(errors)
    file = (review_dir / name).resolve()
    try:
        text = file.read_text(encoding="utf-8", errors="replace")
    except OSError as e:
        raise ReviewInvalid([f"file: {file}: {e.strerror or e}"])
    title = spec.get("title") or next((t for _, level, t in outline(text) if level == 1), "") or file.name
    return Review(review_dir, spec, file, text, title)


def display_path(file: Path) -> str:
    """The path as the reader knows it: relative to the working directory when under it."""
    try:
        return str(file.relative_to(Path.cwd()))
    except ValueError:
        home = Path.home()
        return "~/" + str(file.relative_to(home)) if file.is_relative_to(home) else str(file)


def payload(review: Review, version: int, oldest: int) -> dict:
    """The JSON the page renders."""
    return {
        "version": version,
        "oldest": oldest,
        "title": review.title,
        "subtitle": review.spec.get("subtitle", ""),
        "file": display_path(review.file),
        "path": str(review.file),
        "sourceUrl": review.spec.get("source_url", ""),
        "text": review.text,
    }


# --------------------------------------------------------------------------
# Questions (see lib/session.py for how they reach the session)


def numbered(lines: list[str], lo: int, hi: int) -> str:
    width = len(str(hi))
    return "\n".join(f"{n:>{width}} | {lines[n - 1]}" for n in range(lo, hi + 1))


def refs_text(review: Review, refs: list[dict]) -> str:
    out = []
    for r in refs:
        quoted = "> " + r["text"].replace("\n", "\n> ")
        if r.get("lo"):
            span = f"line {r['lo']}" if r["lo"] == r["hi"] else f"lines {r['lo']}–{r['hi']}"
            out.append(
                f"Selected text ({span} of {display_path(review.file)}):\n{quoted}\n"
                f"Source, {span}:\n```markdown\n{numbered(review.lines, r['lo'], r['hi'])}\n```"
            )
        else:
            out.append(f"Selected text:\n{quoted}")
    return "\n\n".join(out)


# --------------------------------------------------------------------------
# Server


class State(session.State):
    error_event = "doc-error"

    def __init__(self, review: Review, token: str):
        super().__init__(review.dir, token)
        self.review = review
        self.versions: dict[int, str] = {1: review.text}

    def engine(self) -> str:
        return engine()

    def assets(self) -> dict[str, Path]:
        return asset_files()

    def payload(self) -> dict:
        return {**payload(self.review, self.version, min(self.versions)), "status": self.status(), "engine": engine()}

    def sources(self) -> list[Path]:
        return self.review.sources

    def reload(self):
        try:
            review = load_review(self.dir)
        except ReviewInvalid as e:
            self.errors = e.errors
            self.broadcast("doc-error", {"errors": e.errors})
            log(f"reload failed: {len(e.errors)} error(s)")
            return
        same = review.text == self.review.text and review.spec == self.review.spec
        self.review, self.errors = review, []
        if same:
            return
        self.version += 1
        self.versions[self.version] = review.text
        for v in sorted(self.versions)[:-VERSIONS_KEPT]:
            del self.versions[v]
        self.broadcast("doc", {"version": self.version})
        log(f"document reloaded (version {self.version})")


class Handler(session.Handler):
    server_version = "prose-review"
    state: State

    def get(self, route: str) -> bool:
        st = self.state
        if route == "/api/doc":
            self._json(st.payload())
        elif route.startswith("/api/version/"):
            try:
                text = st.versions[int(route.rsplit("/", 1)[1])]
            except (KeyError, ValueError):
                self._error(404, "that version is no longer kept")
            else:
                self._json({"text": text})
        else:
            return False
        return True

    def posts(self):
        return {"/api/ask": self._ask}

    def thread_key(self, body: dict) -> str:
        return THREAD

    def _refs(self, review: Review, refs) -> list[dict]:
        out = []
        n = len(review.lines)
        for r in refs if isinstance(refs, list) else []:
            if not isinstance(r, dict) or r.get("kind") != "passage":
                continue
            if not isinstance(r.get("text"), str) or not r["text"].strip():
                continue
            text = r["text"].strip()[:2000]
            snippet = re.sub(r"[*`_]+", "", " ".join(text.split()))[:60]
            ref = {"kind": "passage", "text": text, "lo": None, "hi": None, "label": snippet}
            try:
                lo, hi = sorted((int(r["lo"]), int(r["hi"])))
            except (KeyError, TypeError, ValueError):
                lo = hi = 0
            if 1 <= lo <= n:
                lo, hi = lo, min(hi, n)
                ref.update(lo=lo, hi=hi, label=f"L{lo}{'' if lo == hi else f'–{hi}'} {snippet}")
            out.append(ref)
        return out[:8]

    def _ask(self, body: dict):
        review = self.state.review
        text = body.get("text")
        refs = self._refs(review, body.get("refs"))
        if not isinstance(text, str) or not (text.strip() or refs):
            return self._error(400, "expected a question")
        qid = new_id()
        msg = {"id": qid, "role": "user", "text": text.strip()[:20_000], "refs": refs,
               "at": time.time(), "state": "queued"}
        item = {
            "id": qid,
            "kind": "question",
            "file": str(review.file),
            "text": msg["text"],
            "refs": refs_text(review, refs),
            "at": msg["at"],
        }
        self.add_question(THREAD, msg, item)


# --------------------------------------------------------------------------
# Commands


def print_report(review: Review):
    heads = outline(review.text)
    words = len(review.text.split())
    print(f"{display_path(review.file)}: {len(review.lines)} lines, about {words} words, {len(heads)} headings")
    width = len(str(len(review.lines)))
    for line, level, title in heads:
        print(f"  {line:>{width}}  {'#' * level} {title}")


def cmd_check(args):
    try:
        review = load_review(Path(args.review))
    except ReviewInvalid as e:
        for err in e.errors:
            print(f"error: {err}")
        sys.exit(1)
    print_report(review)


def load_or_exit(review_dir: Path) -> Review:
    try:
        return load_review(review_dir)
    except ReviewInvalid as e:
        sys.exit("\n".join(f"error: {err}" for err in e.errors))


def cmd_serve(args):
    review_dir = Path(args.review).resolve()
    session.serve(args, review_dir, entry=SCRIPT, engine=engine(), load=lambda: load_or_exit(review_dir), start=State,
                  handler=Handler, report=lambda st: print_report(st.review))


def cmd_stop(args):
    session.stop(Path(args.review).resolve())


def cmd_listen(args):
    review_dir = Path(args.review).resolve()
    q = Queue(review_dir)
    me = shlex_path(SCRIPT)
    review_arg = shlex_path(review_dir)
    if args.answer:
        if not QUESTION_ID.match(args.answer) or not q.file(args.answer).exists():
            sys.exit(f"no open question {args.answer} (answered or withdrawn already?)")
        write_atomic(q.file(args.answer, ".answer.md"), session.read_answer(args))
        print(f"Posted the answer to {args.answer}.")
    session.check_server(review_dir, SCRIPT, engine(), "review")
    item, pending = session.wait_for_item(q, args.timeout, skip=lambda x: x["id"] == args.answer)

    wait = "" if args.timeout == 100 else f" --timeout {args.timeout:g}"
    again = f"python3 {me} listen {review_arg}{wait}"
    if item is None:
        return session.nothing_asked(q, args.timeout, "document", again)
    rest = len(pending) - 1
    if item.get("kind") == "finished":
        q.remove(item["id"])
        print("The reader marked the review finished.")
        if rest:
            print(f"{rest} question(s) still wait for answers; keep listening with:\n  {again}")
        return
    q.claim(item["id"])
    print(f"Question {item['id']} about {item['file']}:\n")
    if item.get("refs"):
        print(item["refs"] + "\n")
    print(item["text"] or "(No text; the reader wants the selection discussed.)")
    print(
        "\nAnswer for the chat panel: concise Markdown. If the reader asks for a change, edit the"
        " file in place (the page reloads and marks what changed) and say in the answer what you"
        " changed. Write the answer to a file, then post it and keep listening:\n"
        f"  python3 {me} listen {review_arg} --answer {item['id']} --file ANSWER.md{wait}"
    )
    if rest:
        print(f"({rest} more question(s) waiting.)")


def main(argv=None):
    p = argparse.ArgumentParser(
        description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter
    )
    sub = p.add_subparsers(dest="cmd", required=True)

    c = sub.add_parser("check", help="validate a review directory and print the document's outline")
    c.add_argument("review", help="review directory")
    c.set_defaults(func=cmd_check)

    s = sub.add_parser(
        "serve",
        help="serve the document as a local web page",
        description="Serve the document. The review directory keeps its token, port and these options for later "
        "servers; a server running another prose_review.py or other options is restarted in place.",
    )
    s.add_argument("review")
    session.add_serve_flags(s)
    s.set_defaults(func=cmd_serve)

    l = sub.add_parser("listen", help="wait for the reader's next question")
    l.add_argument("review")
    l.add_argument("--answer", metavar="ID", help="first post the answer to this question")
    l.add_argument("--file", help="answer file (default: read the answer from stdin)")
    l.add_argument("--timeout", type=float, default=100, help="seconds to wait (default 100)")
    l.set_defaults(func=cmd_listen)

    t = sub.add_parser("stop", help="stop a detached server")
    t.add_argument("review")
    t.set_defaults(func=cmd_stop)

    args = p.parse_args(argv)
    args.func(args)


if __name__ == "__main__":
    main()
