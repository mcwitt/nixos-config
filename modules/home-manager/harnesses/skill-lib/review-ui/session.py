"""The server side of a page that a reader reads beside a chat panel, used
by walkthrough's deck.py.

Each skill reaches this directory through its `lib` symlink, which the
installed skill holds as a real copy. A skill supplies the content: it
subclasses State and Handler, and builds its commands from serve(), stop(),
check_server() and wait_for_item().

Questions cross between the server and `listen` only as files in the
directory's `queue/`: the page posts a question and the server writes
queue/<id>.json; `listen` hands the oldest one to the session, marks it
claimed (<id>.claimed), and later writes the session's answer to
<id>.answer.md, which the server moves into the thread. The server keeps
`threads.json` (the chat history), `server.json` (the running server),
`.server-address` (the token, port and options a restart reuses) and
`server.log` beside it. Requires Python >= 3.11; standard library only.
"""

from __future__ import annotations

import getpass
import hashlib
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
from collections.abc import Callable, Iterable
from http.server import BaseHTTPRequestHandler, ThreadingHTTPServer
from pathlib import Path
from urllib.parse import urlsplit

LIB_ASSETS = Path(__file__).resolve().parent / "assets"
QUESTION_ID = re.compile(r"^q-[0-9a-z-]+$")
HEARTBEAT = 15.0
# The session re-runs `listen` after each timeout; treat it as listening for
# this long after the last run so the page does not flicker between runs.
LISTEN_GRACE = 90.0
CLAIM_STALE = 20 * 60


# --------------------------------------------------------------------------
# Files


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


def log(msg: str):
    print(time.strftime("%H:%M:%S"), msg, file=sys.stderr, flush=True)


def shlex_path(p: Path) -> str:
    s = str(p)
    return s if re.fullmatch(r"[\w./~+-]+", s) else "'" + s.replace("'", "'\\''") + "'"


def ago(ts: float) -> str:
    minutes = int((time.time() - ts) // 60)
    return "just now" if minutes < 1 else f"{minutes} min ago"


def page_files(dirs: Iterable[Path]) -> dict[str, Path]:
    """The files a page may load, by name; the directories share one namespace, so names must differ."""
    files = [p for d in dirs for p in sorted(d.iterdir()) if p.is_file() and not p.name.startswith(".")]
    names = [p.name for p in files]
    if len(set(names)) != len(names):
        raise SystemExit(f"page assets with the same name: {sorted({n for n in names if names.count(n) > 1})}")
    return {p.name: p for p in files}


def digest(paths: Iterable[Path]) -> str:
    """A digest of a server's code and page files: a deploy changes it, which marks a running server out of date."""
    h = hashlib.sha256()
    for p in [*paths, Path(__file__).resolve()]:
        h.update(p.name.encode() + b"\0" + p.read_bytes())
    return h.hexdigest()[:12]


class Queue:
    def __init__(self, directory: Path):
        self.dir = directory / "queue"

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

    def claim(self, qid: str):
        write_atomic(self.file(qid, ".claimed"), json.dumps({"pid": os.getpid(), "at": time.time()}))

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


class Threads:
    """Chat history by thread (a slide, or the whole document), persisted to threads.json. Only the server writes it."""

    def __init__(self, path: Path):
        self.path = path
        self.lock = threading.Lock()
        try:
            self.data = json.loads(path.read_text()).get("threads", {})
        except (OSError, ValueError):
            self.data = {}

    def save(self):
        write_atomic(self.path, json.dumps({"version": 1, "threads": self.data}, indent=1))

    def messages(self, thread: str) -> list[dict]:
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


# --------------------------------------------------------------------------
# Server


class State:
    """A served directory: the open pages, the question queue, the threads and the session's presence.
    A skill subclasses it for its content and overrides the hooks below."""

    error_event = "error"  # the event that tells the pages the content on disk does not load
    # Queue items whose claim means the session is answering; the server moves answers to questions into threads.
    claimable = ("question",)

    def __init__(self, directory: Path, token: str):
        self.dir = directory
        self.token = token
        self.queue = Queue(directory)
        self.queue.dir.mkdir(exist_ok=True)
        self.threads = Threads(directory / "threads.json")
        self.version = 1
        self.errors: list[str] = []
        self.mtimes: dict[Path, int] = {}
        self.listeners: list[queue.Queue] = []
        self.listeners_lock = threading.Lock()
        self.presence = "away"
        self.last_listener = 0.0
        self.busy = False
        self.last_activity = time.monotonic()
        self.last_page = time.time()

    # Hooks

    def engine(self) -> str:
        raise NotImplementedError

    def assets(self) -> dict[str, Path]:
        raise NotImplementedError

    def payload(self) -> dict:
        """The content as the page renders it."""
        raise NotImplementedError

    def sources(self) -> list[Path]:
        """The files whose changes reload() picks up."""
        return []

    def reload(self):
        """A source changed: load the content again, or set self.errors and tell the pages."""

    def sync_item(self, item: dict):
        """Follow a claimable item other than a question."""

    def synced(self, items: list[dict]):
        """Called after each pass over the queue, with its items."""

    def working(self) -> bool:
        """Whether the session is busy with something other than a question."""
        return False

    # Shared

    def status(self) -> dict:
        return {"session": self.presence}

    def _mtimes(self) -> dict[Path, int]:
        out = {}
        for p in self.sources():
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
        """Pick up edits to the sources, claims and answers from the session."""
        self.mtimes = self._mtimes()
        tick = 0
        while True:
            time.sleep(0.5)
            tick += 1
            working = False
            try:
                self.sync_queue()
                working = self.working()
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
            if tick % 2 == 0 and self._mtimes() != self.mtimes:
                time.sleep(0.3)  # let a multi-file edit settle
                try:
                    self.reload()
                except Exception as e:  # the content shown stands; the log says what failed
                    log(f"reload: {e!r}")
                # After the reload: it may have changed the list of sources.
                self.mtimes = self._mtimes()

    def sync_queue(self):
        changed = set()
        busy = False
        items = self.queue.items()
        for item in items:
            qid = item["id"]
            if item.get("kind") not in self.claimable:
                continue
            busy |= self.queue.claim_age(qid) < CLAIM_STALE
            if item["kind"] != "question":
                self.sync_item(item)
                continue
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
                    changed.add(thread)
                elif self.queue.claimed(qid) and msg.get("state") == "queued":
                    msg["state"] = "claimed"
                    self.threads.save()
                    changed.add(thread)
        self.busy = busy
        self.synced(items)
        for thread in changed:
            self.broadcast("thread", {"slide": thread})


class ClientGone(Exception):
    pass


class Handler(BaseHTTPRequestHandler):
    """Serves the page, its assets, the threads and the event stream, and takes questions.
    A skill subclasses it, adds its routes in get() and posts(), and posts questions with add_question()."""

    server_version = "review-ui"
    state: State

    def log_message(self, fmt, *args):
        pass

    # Hooks

    def get(self, route: str) -> bool:
        """Answer a GET route of the skill's own; return whether it did."""
        return False

    def posts(self) -> dict[str, Callable[[dict], None]]:
        """The skill's own POST routes."""
        return {}

    def post(self, route: str, body: dict) -> bool:
        """Answer a POST route not in posts(); return whether it did."""
        return False

    def page(self, name: str, body: bytes) -> bytes:
        """An asset as served; the skill may rewrite it."""
        return body

    def thread_key(self, body: dict) -> str:
        """The thread that a request to clear names."""
        return str(body.get("slide"))

    # Shared

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
        if route == "/api/threads":
            return self._json(st.threads.snapshot())
        if route == "/api/events":
            return self._events()
        if self.get(route):
            return
        name = route.lstrip("/")
        if name in st.assets():
            return self._static(name)
        self._error(404, "not found")

    def _static(self, name: str):
        types = {".html": "text/html", ".css": "text/css", ".js": "text/javascript"}
        ctype = types.get(Path(name).suffix, "application/octet-stream") + "; charset=utf-8"
        self._send(200, self.page(name, self.state.assets()[name].read_bytes()), ctype)

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
            self._sse("hello", {"version": st.version, "status": st.status(), "engine": st.engine()})
            if st.errors:
                self._sse(st.error_event, {"errors": st.errors})
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
            "/api/withdraw": self._withdraw,
            "/api/clear": self._clear,
            "/api/finish": self._finish,
            **self.posts(),
        }.get(route)
        if handler:
            return handler(body)
        if not self.post(route, body):
            self._error(404, "not found")

    def add_question(self, thread: str, msg: dict, item: dict):
        """Add the reader's message to its thread and queue it for `listen`; answer the page."""
        st = self.state
        with st.threads.lock:
            st.threads.messages(thread).append(msg)
            st.threads.save()
            st.queue.add(item)
        st.broadcast("thread", {"slide": thread})
        self._json({"message": msg})

    def _withdraw(self, body: dict):
        st = self.state
        qid = str(body.get("id"))
        with st.threads.lock:
            thread, msg = st.threads.find(qid)
            if msg is None:
                return self._error(404, "no such question")
            if msg.get("state") != "queued" or st.queue.claimed(qid):
                return self._error(409, "the session is already answering it")
            st.queue.remove(qid)
            msg["state"] = "withdrawn"
            st.threads.save()
        st.broadcast("thread", {"slide": thread})
        self._json({"ok": True})

    def _clear(self, body: dict):
        st = self.state
        thread = self.thread_key(body)
        with st.threads.lock:
            msgs = st.threads.data.get(thread, {}).get("messages", [])
            if any(m.get("state") in ("queued", "claimed") for m in msgs):
                return self._error(409, "a question here is still waiting for an answer")
            st.threads.data.pop(thread, None)
            st.threads.save()
        st.broadcast("thread", {"slide": thread})
        self._json({"ok": True})

    def _finish(self, body: dict):
        self.state.queue.add({"id": new_id(), "kind": "finished", "at": time.time()})
        self._json({"ok": True})


# --------------------------------------------------------------------------
# Commands

# Options a directory keeps between servers, with their defaults.
SERVE_OPTIONS = {"host": "127.0.0.1", "port": 0, "public_host": None, "idle_hours": 12.0, "tunnel": None}


def add_serve_flags(parser):
    """The flags of `serve`; a flag left out keeps the value the directory's last server had."""
    parser.add_argument("--host", help="address to listen on (default 127.0.0.1)")
    parser.add_argument("--port", type=int, help="port (default: the previous one, or any free port)")
    parser.add_argument("--public-host", help="host name to print in the URL")
    parser.add_argument("--idle-hours", type=float, help="exit after this long unused (default 12)")
    parser.add_argument(
        "--tunnel",
        nargs="?",
        const="",
        metavar="DEST",
        help="print the ssh command that forwards the port from the reader's machine, which reaches this "
        "one as ssh DEST (default USER@HOSTNAME); 'off' stops printing it",
    )
    parser.add_argument("--detach", action="store_true", help="run in the background; print the URL")


def serve_options(args, saved: dict) -> dict:
    """The saved options, overridden by the flags given on this command line."""
    given = {k: getattr(args, k) for k in SERVE_OPTIONS if getattr(args, k, None) is not None}
    opts = {**SERVE_OPTIONS, **saved, **given}
    if opts["tunnel"] == "":
        opts["tunnel"] = f"{getpass.getuser()}@{socket.gethostname()}"
    elif opts["tunnel"] == "off":
        opts["tunnel"] = None
    return opts


def server_info(directory: Path) -> dict | None:
    info = read_json(directory / "server.json")
    return info if pid_alive(info.get("pid")) else None


def stale_reason(info: dict, entry: Path, engine: str) -> str | None:
    """Why a running server is not `entry` with these page assets, if it is not."""
    if info.get("engine") == engine:
        return None
    if info.get("script") == str(entry):
        return f"{entry.name} or its page changed since it started"
    return f"it runs {info.get('script', 'an older script')}"


def print_address(info: dict):
    print(info["url"])
    if info.get("tunnel"):
        print(f"From another machine, forward the port first: {info['tunnel']}")


def stop_server(directory: Path, info: dict):
    try:
        # A server runs the script that started it; one from before `script` was recorded ran this one.
        script = Path(info.get("script") or sys.argv[0]).name
        if not script.endswith(".py") or script.encode() not in Path(f"/proc/{info['pid']}/cmdline").read_bytes():
            sys.exit(f"pid {info['pid']} is not a {script} server; remove {directory / 'server.json'}")
    except OSError:
        pass  # no /proc (macOS); trust server.json
    os.kill(info["pid"], signal.SIGTERM)
    # Wait for the process to exit, not just for server.json to go: older servers removed it before freeing the port.
    for _ in range(50):
        time.sleep(0.1)
        if not pid_alive(info["pid"]):
            (directory / "server.json").unlink(missing_ok=True)
            return
    sys.exit("the server did not stop")


def serve(args, directory: Path, *, entry: Path, engine: str, load: Callable[[], object],
          start: Callable[[object, str], State], handler: type[Handler], report: Callable[[State], None],
          info: dict | None = None):
    """Serve a directory, or restart its server in place when that server is out of date or has other options.

    load() validates the content and exits when it does not load; start(content, token) makes the State.
    """
    saved = directory / ".server-address"
    address = read_json(saved)
    opts = serve_options(args, address.get("options", {}))
    if opts["tunnel"] and opts["host"] not in ("127.0.0.1", "localhost", "::1"):
        sys.exit("--tunnel forwards a loopback port; leave out --host, or pass --tunnel off")
    running = server_info(directory)
    # A current server keeps serving the last content that loaded while the sources have errors.
    if running and not stale_reason(running, entry, engine) and running.get("options") == opts:
        print("already serving:")
        print_address(running)
        return
    content = load()
    if running:
        # Restart in place: the token and port carry over, so open pages reconnect and reload.
        stop_server(directory, running)
        print(f"restarted the server: {stale_reason(running, entry, engine) or 'new options'}")
    if args.detach:
        argv = [a for a in sys.argv[1:] if a != "--detach"]
        with open(directory / "server.log", "ab") as logf:
            proc = subprocess.Popen(
                [sys.executable, str(entry), *argv],
                stdin=subprocess.DEVNULL,
                stdout=logf,
                stderr=logf,
                # Its own session: no terminal, so closing the terminal or tmux session sends it no SIGHUP.
                start_new_session=True,
            )
        for _ in range(150):
            time.sleep(0.1)
            now = server_info(directory)
            if now and now["pid"] == proc.pid:
                print_address(now)
                return
            if proc.poll() is not None:
                break
        sys.exit(f"the server did not start; see {directory / 'server.log'}")

    sys.stdout.reconfigure(line_buffering=True)  # keep server.log in order
    token = address.get("token") or secrets.token_urlsafe(18)
    state = start(content, token)
    handler.state = state
    # Reuse the previous port so the URL survives a restart, unless --port names one.
    try:
        httpd = ThreadingHTTPServer((opts["host"], opts["port"] or address.get("port", 0)), handler)
    except OSError:
        httpd = ThreadingHTTPServer((opts["host"], opts["port"]), handler)
    httpd.daemon_threads = True
    host, port = httpd.server_address[:2]
    shown = opts["public_host"] or (socket.gethostname() if host in ("0.0.0.0", "::") else host)
    record = {
        "pid": os.getpid(),
        "url": f"http://{shown}:{port}/{token}/",
        "port": port,
        "tunnel": f"ssh -N -L {port}:127.0.0.1:{port} {opts['tunnel']}" if opts["tunnel"] else None,
        "engine": engine,
        "script": str(entry),
        **(info or {}),
        "options": opts,
    }
    write_atomic(directory / "server.json", json.dumps(record), 0o600)
    write_atomic(saved, json.dumps({"token": token, "port": port, "options": opts}), 0o600)
    state.pages_changed()
    report(state)
    print_address(record)
    log(f"serving {record['url']} (pid {os.getpid()}, engine {engine} at {entry})")
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
        (directory / "server.json").unlink(missing_ok=True)
        log("stopped")


def stop(directory: Path):
    info = server_info(directory)
    if not info:
        (directory / "server.json").unlink(missing_ok=True)
        print("not running")
        return
    stop_server(directory, info)
    print("stopped")


def read_answer(args) -> str:
    """The answer `listen --answer` posts, from --file or stdin."""
    text = Path(args.file).read_text() if args.file else sys.stdin.read()
    if not text.strip():
        sys.exit("the answer is empty")
    return text


def check_server(directory: Path, entry: Path, engine: str, noun: str,
                 reject: Callable[[dict], str | None] = lambda info: None) -> dict:
    """The running server, for `listen`: exit when there is none or reject() names a reason; warn when it is out of date."""
    info = server_info(directory)
    serve_cmd = f"python3 {shlex_path(entry)} serve {shlex_path(directory)} --detach"
    if not info:
        sys.exit(f"No server is running for this {noun}. Start it with:\n  {serve_cmd}")
    why = reject(info)
    if why:
        sys.exit(why)
    reason = stale_reason(info, entry, engine)
    if reason:
        print(f"The server is out of date: {reason}. "
              f"Restart it in place; the URL stays the same and open pages reload:\n  {serve_cmd}\n")
    return info


def wait_for_item(q: Queue, timeout: float, skip: Callable[[dict], bool] = lambda x: False,
                  rank: Callable[[dict], int] = lambda x: 0) -> tuple[dict | None, list[dict]]:
    """Wait for the next unanswered queue item that skip() keeps, marking the session as listening meanwhile.

    Items the session claimed but has not answered come first: a session that lost track of one (an
    interruption, a context reset) gets it again. Then lower rank() first, then older first.
    """
    q.dir.mkdir(exist_ok=True)
    marker = q.dir / "listening.json"
    deadline = time.time() + timeout
    write_atomic(marker, json.dumps({"pid": os.getpid(), "until": deadline + 5}))
    pending: list[dict] = []
    try:
        while True:
            pending = [x for x in q.items() if not q.answered(x["id"]) and not skip(x)]
            pending.sort(key=lambda x: (not q.claimed(x["id"]), rank(x), x.get("at", 0)))
            if pending:
                return pending[0], pending
            if time.time() >= deadline:
                return None, pending
            time.sleep(0.5)
    finally:
        try:
            if json.loads(marker.read_text()).get("pid") == os.getpid():
                marker.unlink()
        except (OSError, ValueError):
            pass


def nothing_asked(q: Queue, timeout: float, noun: str, again: str):
    pages = q.pages()
    if pages.get("open"):
        seen = f"The {noun} is open in {pages['open']} page(s)."
    else:
        seen = f"No page has the {noun} open (last seen {ago(pages.get('last_seen', 0))})."
    print(f"No questions in the last {timeout:g} s. {seen}\nKeep listening with:\n  {again}")
