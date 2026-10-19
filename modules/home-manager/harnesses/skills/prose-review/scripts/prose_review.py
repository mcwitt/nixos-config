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
`listen` needs no network access. Requires Python >= 3.11; uses only the
standard library.
"""

from __future__ import annotations

import argparse
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
from dataclasses import dataclass
from http.server import BaseHTTPRequestHandler, ThreadingHTTPServer
from pathlib import Path
from urllib.parse import urlsplit

ASSETS = Path(__file__).resolve().parent.parent / "assets"
SCRIPT = Path(__file__).resolve()
QUESTION_ID = re.compile(r"^q-[0-9a-z-]+$")
HEADING_RE = re.compile(r"^ {0,3}(#{1,6})\s+(.*?)\s*#*\s*$")
FENCE_RE = re.compile(r"^ {0,3}(`{3,}|~{3,})")
THREAD = "doc"  # one conversation per review
HEARTBEAT = 15.0
# The session re-runs `listen` after each timeout; treat it as listening for
# this long after the last run so the page does not flicker between runs.
LISTEN_GRACE = 90.0
CLAIM_STALE = 20 * 60
VERSIONS_KEPT = 50


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
# Questions
#
# The page posts a question and the server writes queue/<id>.json. `listen`
# hands the oldest unanswered question to the review session, marks it
# claimed (<id>.claimed), and later writes the session's answer to
# <id>.answer.md, which the server moves into the thread.


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
    def __init__(self, review_dir: Path):
        self.dir = review_dir / "queue"

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


class Threads:
    """Chat history, persisted to threads.json. Only the server writes it."""

    def __init__(self, path: Path):
        self.path = path
        self.lock = threading.Lock()
        try:
            self.data = json.loads(path.read_text()).get("threads", {})
        except (OSError, ValueError):
            self.data = {}

    def save(self):
        write_atomic(self.path, json.dumps({"version": 1, "threads": self.data}, indent=1))

    def messages(self, thread: str = THREAD) -> list[dict]:
        return self.data.setdefault(thread, {"messages": []})["messages"]

    def find(self, qid: str) -> tuple[str, dict] | tuple[None, None]:
        for thread, t in self.data.items():
            for m in t["messages"]:
                if m.get("id") == qid and m["role"] == "user":
                    return thread, m
        return None, None

    def snapshot(self) -> dict:
        with self.lock:
            return json.loads(json.dumps({k: v for k, v in self.data.items() if v["messages"]}))


class State:
    def __init__(self, review_dir: Path, token: str):
        self.dir = review_dir
        self.token = token
        self.queue = Queue(review_dir)
        self.queue.dir.mkdir(exist_ok=True)
        self.threads = Threads(review_dir / "threads.json")
        self.review = load_review(review_dir)
        self.version = 1
        self.versions: dict[int, str] = {1: self.review.text}
        self.errors: list[str] = []
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
        return {**payload(self.review, self.version, min(self.versions)), "status": self.status()}

    def _mtimes(self) -> dict[Path, int]:
        out = {}
        for p in self.review.sources:
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
        """Pick up edits to the document, claims and answers from the review session."""
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
                self._reload()

    def _reload(self):
        if self._mtimes() == self.mtimes:
            return
        time.sleep(0.3)  # let an edit settle
        try:
            review = load_review(self.dir)
        except ReviewInvalid as e:
            self.mtimes = self._mtimes()
            self.errors = e.errors
            self.broadcast("doc-error", {"errors": e.errors})
            log(f"reload failed: {len(e.errors)} error(s)")
            return
        same = review.text == self.review.text and review.spec == self.review.spec
        self.review, self.errors = review, []
        self.mtimes = self._mtimes()
        if same:
            return
        self.version += 1
        self.versions[self.version] = review.text
        for v in sorted(self.versions)[:-VERSIONS_KEPT]:
            del self.versions[v]
        self.broadcast("doc", {"version": self.version})
        log(f"document reloaded (version {self.version})")

    def _sync_queue(self):
        changed = False
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
                thread, msg = self.threads.find(qid)
                if msg is None:
                    continue
                if self.queue.answered(qid):
                    text = self.queue.file(qid, ".answer.md").read_text()
                    msg["state"] = "answered"
                    self.threads.messages(thread).append(
                        {"id": qid + "-a", "role": "assistant", "text": text, "at": time.time()}
                    )
                    self.threads.save()
                    self.queue.remove(qid)
                    changed = True
                elif self.queue.claimed(qid) and msg.get("state") == "queued":
                    msg["state"] = "claimed"
                    self.threads.save()
                    changed = True
        self.busy = busy
        if changed:
            self.broadcast("thread", {})


def log(msg: str):
    print(time.strftime("%H:%M:%S"), msg, file=sys.stderr, flush=True)


class ClientGone(Exception):
    pass


class Handler(BaseHTTPRequestHandler):
    server_version = "prose-review"
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
        if route == "/api/doc":
            return self._json(st.payload())
        if route == "/api/threads":
            return self._json(st.threads.snapshot())
        if route.startswith("/api/version/"):
            try:
                text = st.versions[int(route.rsplit("/", 1)[1])]
            except (KeyError, ValueError):
                return self._error(404, "that version is no longer kept")
            return self._json({"text": text})
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
            if st.errors:
                self._sse("doc-error", {"errors": st.errors})
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
            "/api/finish": self._finish,
        }.get(route)
        if handler is None:
            return self._error(404, "not found")
        handler(body)

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
        st = self.state
        review = st.review
        text = body.get("text")
        refs = self._refs(review, body.get("refs"))
        if not isinstance(text, str) or not (text.strip() or refs):
            return self._error(400, "expected a question")
        qid = f"q-{int(time.time() * 1000):x}-{secrets.token_hex(2)}"
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
        with st.threads.lock:
            st.threads.messages().append(msg)
            st.threads.save()
            st.queue.add(item)
        st.broadcast("thread", {})
        self._json({"message": msg})

    def _withdraw(self, body: dict):
        st = self.state
        qid = str(body.get("id"))
        with st.threads.lock:
            _, msg = st.threads.find(qid)
            if msg is None:
                return self._error(404, "no such question")
            if msg.get("state") != "queued" or st.queue.claimed(qid):
                return self._error(409, "the review session is already answering it")
            st.queue.remove(qid)
            msg["state"] = "withdrawn"
            st.threads.save()
        st.broadcast("thread", {})
        self._json({"ok": True})

    def _clear(self, body: dict):
        st = self.state
        with st.threads.lock:
            msgs = st.threads.data.get(THREAD, {}).get("messages", [])
            if any(m.get("state") in ("queued", "claimed") for m in msgs):
                return self._error(409, "a question is still waiting for an answer")
            st.threads.data.pop(THREAD, None)
            st.threads.save()
        st.broadcast("thread", {})
        self._json({"ok": True})

    def _finish(self, body: dict):
        st = self.state
        qid = f"q-{int(time.time() * 1000):x}-{secrets.token_hex(2)}"
        st.queue.add({"id": qid, "kind": "finished", "at": time.time()})
        self._json({"ok": True})


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


def server_info(review_dir: Path) -> dict | None:
    try:
        info = json.loads((review_dir / "server.json").read_text())
    except (OSError, ValueError):
        return None
    return info if pid_alive(info.get("pid")) else None


def cmd_serve(args):
    review_dir = Path(args.review).resolve()
    running = server_info(review_dir)
    if running:
        print(f"already serving: {running['url']}")
        return
    if args.detach:
        argv = [a for a in sys.argv[1:] if a != "--detach"]
        with open(review_dir / "server.log", "ab") as logf:
            proc = subprocess.Popen(
                [sys.executable, str(SCRIPT), *argv],
                stdin=subprocess.DEVNULL,
                stdout=logf,
                stderr=logf,
                start_new_session=True,
            )
        for _ in range(150):
            time.sleep(0.1)
            info = server_info(review_dir)
            if info and info["pid"] == proc.pid:
                print(info["url"])
                return
            if proc.poll() is not None:
                break
        sys.exit(f"the server did not start; see {review_dir / 'server.log'}")

    # Reuse the previous token and port, so a restarted server keeps the URL
    # and open pages reconnect on their own.
    saved = review_dir / ".server-address"
    try:
        token, port = json.loads(saved.read_text()).values()
    except (OSError, ValueError):
        token, port = secrets.token_urlsafe(18), 0
    try:
        state = State(review_dir, token)
    except ReviewInvalid as e:
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
    write_atomic(review_dir / "server.json", json.dumps({"pid": os.getpid(), "url": url, "port": port}), 0o600)
    write_atomic(saved, json.dumps({"token": state.token, "port": port}), 0o600)
    state.pages_changed()
    print_report(state.review)
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
        (review_dir / "server.json").unlink(missing_ok=True)


def cmd_stop(args):
    review_dir = Path(args.review).resolve()
    info = server_info(review_dir)
    if not info:
        (review_dir / "server.json").unlink(missing_ok=True)
        print("not running")
        return
    try:
        if b"prose_review" not in Path(f"/proc/{info['pid']}/cmdline").read_bytes():
            sys.exit(f"pid {info['pid']} is not a review server; remove {review_dir / 'server.json'}")
    except OSError:
        pass  # no /proc (macOS); trust server.json
    os.kill(info["pid"], signal.SIGTERM)
    for _ in range(50):
        time.sleep(0.1)
        if not server_info(review_dir):
            print("stopped")
            return
    sys.exit("the server did not stop")


def _ago(ts: float) -> str:
    minutes = int((time.time() - ts) // 60)
    return "just now" if minutes < 1 else f"{minutes} min ago"


def cmd_listen(args):
    review_dir = Path(args.review).resolve()
    q = Queue(review_dir)
    me = shlex_path(SCRIPT)
    review_arg = shlex_path(review_dir)
    if args.answer:
        if not QUESTION_ID.match(args.answer) or not q.file(args.answer).exists():
            sys.exit(f"no open question {args.answer} (answered or withdrawn already?)")
        text = Path(args.file).read_text() if args.file else sys.stdin.read()
        if not text.strip():
            sys.exit("the answer is empty")
        write_atomic(q.file(args.answer, ".answer.md"), text)
        print(f"Posted the answer to {args.answer}.")
    if not server_info(review_dir):
        sys.exit(f"No server is running for this review. Start it with:\n  python3 {me} serve {review_arg} --detach")

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
    again = f"python3 {me} listen {review_arg}{wait}"
    if item is None:
        pages = q.pages()
        if pages.get("open"):
            seen = f"The document is open in {pages['open']} page(s)."
        else:
            seen = f"No page has the document open (last seen {_ago(pages.get('last_seen', 0))})."
        print(f"No questions in the last {args.timeout:g} s. {seen}\nKeep listening with:\n  {again}")
        return
    rest = len(pending) - 1
    if item.get("kind") == "finished":
        q.remove(item["id"])
        print("The reader marked the review finished.")
        if rest:
            print(f"{rest} question(s) still wait for answers; keep listening with:\n  {again}")
        return
    write_atomic(q.file(item["id"], ".claimed"), json.dumps({"pid": os.getpid(), "at": time.time()}))
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


def shlex_path(p: Path) -> str:
    s = str(p)
    return s if re.fullmatch(r"[\w./~+-]+", s) else "'" + s.replace("'", "'\\''") + "'"


def main(argv=None):
    p = argparse.ArgumentParser(
        description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter
    )
    sub = p.add_subparsers(dest="cmd", required=True)

    c = sub.add_parser("check", help="validate a review directory and print the document's outline")
    c.add_argument("review", help="review directory")
    c.set_defaults(func=cmd_check)

    s = sub.add_parser("serve", help="serve the document as a local web page")
    s.add_argument("review")
    s.add_argument("--host", default="127.0.0.1", help="address to listen on (default 127.0.0.1)")
    s.add_argument("--port", type=int, default=0, help="port (default: any free port)")
    s.add_argument("--public-host", help="host name to print in the URL")
    s.add_argument("--idle-hours", type=float, default=12, help="exit after this long unused")
    s.add_argument("--detach", action="store_true", help="run in the background; print the URL")
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
