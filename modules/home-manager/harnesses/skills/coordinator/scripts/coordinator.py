#!/usr/bin/env python3
"""Run agent sessions in detached tmux sessions: launch, message, list and close them.

The helper does not know any harness. `launch` takes the agent's command line
after `--` and substitutes whole-argument placeholders: {prompt} (the brief),
{name} (the session name) and {session_id} (a fresh UUID, for harnesses that
accept one), as in

    coordinator.py launch fix-x --dir ~/src/x --prompt-file brief.md -- \\
        claude --name {name} --session-id {session_id} -- {prompt}

Nothing passes through a shell, so briefs need no quoting. Liveness and state
always come from tmux; a launch record per session (command, brief, directory,
session ID) lives under the state directory so a session can be relaunched or
resumed later. The record is never trusted for liveness.

Requires Python >= 3.11 and tmux >= 3.0.
"""

from __future__ import annotations

import argparse
import json
import os
import re
import shutil
import subprocess
import sys
import time
import uuid
from pathlib import Path

PLACEHOLDER = re.compile(r"\{(prompt|name|session_id)\}")
NAME_RE = re.compile(r"^[A-Za-z0-9][A-Za-z0-9_-]{0,47}$")
MAX_PROMPT_BYTES = 100_000  # Linux caps a single argv string at 128 KiB
STATES = ("working", "input", "done")
# Startup and confirmation screens seen in agent TUIs. Typed text or Enter
# there picks a choice instead of reaching the agent; for Claude Code's folder
# trust the highlighted choice is "No, exit". Extend the list as harnesses add
# screens: a miss only means `send` cannot refuse on that screen.
DIALOG_MARKERS = (
    "Enter to confirm",
    "enter confirm",
    "Esc to cancel",
    "Trust this folder",
    "Trust project folder",
    "Do you trust",
    "Hooks need review",
    "Update Available",
)
# Claude Code refuses to start nested inside another Claude Code session.
NESTING_ENV = ("CLAUDECODE", "CLAUDE_CODE_ENTRYPOINT")


def die(msg: str, code: int = 1):
    print(f"coordinator: {msg}", file=sys.stderr)
    sys.exit(code)


# ---------------------------------------------------------------- tmux


def tmux(*args: str, check: bool = True, input: str | None = None) -> str:
    r = subprocess.run(["tmux", *args], capture_output=True, text=True, input=input)
    if check and r.returncode:
        die(f"tmux {args[0]}: {r.stderr.strip() or f'exit {r.returncode}'}")
    return r.stdout


def exists(name: str) -> bool:
    return subprocess.run(["tmux", "has-session", "-t", f"={name}"], capture_output=True).returncode == 0


def pane(name: str) -> str:
    """Target for the active pane of session `name`, matched exactly."""
    return f"={name}:"


def option(name: str, opt: str) -> str:
    return tmux("display-message", "-p", "-t", pane(name), f"#{{{opt}}}", check=False).strip()


def current_session() -> str | None:
    if not os.environ.get("TMUX_PANE"):
        return None
    out = subprocess.run(
        ["tmux", "display-message", "-p", "-t", os.environ["TMUX_PANE"], "#{session_name}"],
        capture_output=True,
        text=True,
    )
    return out.stdout.strip() or None


SESSION_FIELDS = (
    "session_name",
    "session_created",
    "session_attached",
    "session_activity",
    "pane_current_path",
    "@agent_state",
)


def sessions() -> list[dict]:
    fmt = "\t".join(f"#{{{f}}}" for f in SESSION_FIELDS)
    r = subprocess.run(["tmux", "list-sessions", "-F", fmt], capture_output=True, text=True)
    if r.returncode:  # no server running means no sessions
        return []
    rows = (line.split("\t") for line in r.stdout.splitlines())
    return [dict(zip(SESSION_FIELDS, parts)) for parts in rows if len(parts) == len(SESSION_FIELDS)]


def capture(name: str, lines: int) -> list[str]:
    text = tmux("capture-pane", "-p", "-J", "-t", pane(name), "-S", f"-{max(lines * 4, 200)}", check=False)
    return [ln.rstrip() for ln in text.splitlines() if ln.strip()][-lines:]


def dialog_open(name: str) -> bool:
    return any(m in ln for ln in capture(name, 15) for m in DIALOG_MARKERS)


# ---------------------------------------------------------------- records


def state_dir() -> Path:
    if d := os.environ.get("COORDINATOR_STATE_DIR"):
        return Path(d)
    base = os.environ.get("XDG_STATE_HOME") or Path.home() / ".local" / "state"
    return Path(base) / "agent-coordinator"


def record_path(name: str) -> Path:
    return state_dir() / "sessions" / f"{name}.json"


def load_record(name: str) -> dict | None:
    try:
        return json.loads(record_path(name).read_text())
    except (OSError, ValueError):
        return None


def save_record(rec: dict):
    p = record_path(rec["name"])
    p.parent.mkdir(parents=True, exist_ok=True)
    tmp = p.with_suffix(".tmp")
    tmp.write_text(json.dumps(rec, indent=2) + "\n")
    tmp.replace(p)


def expand(rec: dict) -> list[str]:
    """The record's command with its placeholders filled in."""
    values = {"prompt": rec.get("prompt") or "", "name": rec["name"], "session_id": rec.get("session_id") or ""}
    # One pass, so a placeholder inside the brief stays as written.
    return [PLACEHOLDER.sub(lambda m: values[m[1]], arg) for arg in rec["command"]]


# ---------------------------------------------------------------- processes


def proc_env(d: Path) -> dict[bytes, bytes]:
    return dict(kv.split(b"=", 1) for kv in (d / "environ").read_bytes().split(b"\0") if b"=" in kv)


def pane_processes() -> list[dict]:
    """Processes of this user that inherited TMUX_PANE from this tmux server (Linux)."""
    socket = tmux("display-message", "-p", "#{socket_path}", check=False).strip()
    if not socket or not Path("/proc/self/environ").exists():
        return []
    procs = []
    for d in Path("/proc").glob("[0-9]*"):
        if int(d.name) == os.getpid():
            continue
        try:
            env = proc_env(d)
            args = (d / "cmdline").read_bytes().replace(b"\0", b" ").decode(errors="replace").strip()
            cwd = os.readlink(d / "cwd")
        except OSError:
            continue
        pane_id = env.get(b"TMUX_PANE", b"").decode()
        if pane_id and env.get(b"TMUX", b"").decode().split(",")[0] == socket:
            procs.append({"pid": int(d.name), "pane": pane_id, "cwd": cwd, "args": args})
    return procs


def stray_processes(panes: set[str] | None = None) -> list[dict]:
    """Processes whose tmux pane is gone, optionally only those from `panes`."""
    live = set(tmux("list-panes", "-a", "-F", "#{pane_id}", check=False).split())
    return [p for p in pane_processes() if p["pane"] not in live and (panes is None or p["pane"] in panes)]


def print_procs(procs: list[dict]):
    for p in procs:
        print(f"  pid {p['pid']}  pane {p['pane']}  cwd {p['cwd']}\n    {p['args'][:200]}")


# ---------------------------------------------------------------- git


def git_toplevel(path: str) -> str | None:
    r = subprocess.run(["git", "-C", path, "rev-parse", "--show-toplevel"], capture_output=True, text=True)
    return r.stdout.strip() if r.returncode == 0 else None


def git_status(path: str) -> str | None:
    r = subprocess.run(["git", "-C", path, "status", "--short"], capture_output=True, text=True)
    return r.stdout.rstrip() if r.returncode == 0 else None


def worktree(repo: str, branch: str, base: str | None) -> str:
    """Path of a worktree for `branch` of `repo`, created if needed.

    Uses worktrunk when installed, so its path template and hooks apply;
    otherwise `git worktree add` with the same <repo>.<branch> sibling layout.
    """
    top = git_toplevel(repo)
    if not top:
        die(f"{repo} is not a Git checkout")
    wt = os.environ.get("COORDINATOR_WT", "wt")
    has_branch = subprocess.run(
        ["git", "-C", top, "rev-parse", "--verify", "--quiet", f"refs/heads/{branch}"], capture_output=True
    ).returncode == 0
    if wt and shutil.which(wt):
        cmd = [wt, "-C", top, "switch", branch, "--no-cd", "--format", "json", "--yes"]
        if not has_branch and not branch.startswith(("pr:", "mr:")):
            cmd.append("--create")
            if base:
                cmd += ["--base", base]
        r = subprocess.run(cmd, capture_output=True, text=True, stdin=subprocess.DEVNULL)
        if r.returncode:
            die(f"wt switch {branch} failed:\n{r.stderr.strip()}")
        info = json.loads(r.stdout.strip().splitlines()[-1])
        print(f"worktree {info['path']} ({info['action']}, branch {info['branch']})")
        return info["path"]
    if branch.startswith(("pr:", "mr:")):
        die(f"{branch} needs worktrunk (wt); fetch the PR head as a branch and pass that instead")
    listing = subprocess.run(["git", "-C", top, "worktree", "list", "--porcelain"], capture_output=True, text=True)
    for block in listing.stdout.split("\n\n"):
        if f"branch refs/heads/{branch}" in block.splitlines():
            path = block.splitlines()[0].removeprefix("worktree ")
            print(f"worktree {path} (existing, branch {branch})")
            return path
    common = subprocess.run(
        ["git", "-C", top, "rev-parse", "--path-format=absolute", "--git-common-dir"], capture_output=True, text=True
    )
    main = Path(common.stdout.strip()).parent
    path = str(main.parent / f"{main.name}.{re.sub(r'[^A-Za-z0-9._-]', '-', branch)}")
    cmd = ["git", "-C", top, "worktree", "add", path]
    cmd += [branch] if has_branch else ["-b", branch] + ([base] if base else [])
    r = subprocess.run(cmd, capture_output=True, text=True)
    if r.returncode:
        die(f"git worktree add failed:\n{r.stderr.strip()}")
    print(f"worktree {path} (created, branch {branch})")
    return path


def check_shared_checkout(name: str, directory: str, share: bool):
    """Refuse to start a second live session in the same Git checkout without --share."""
    top = git_toplevel(directory)
    if not top or share:
        return
    users = [
        s["session_name"]
        for s in sessions()
        if s["session_name"] != name and s["pane_current_path"] and git_toplevel(s["pane_current_path"]) == top
    ]
    if users:
        die(
            f"{top} is already the working directory of session(s) {', '.join(users)}. "
            "Use a separate worktree for a session that edits or checks out, "
            "or pass --share if both sessions only read."
        )


# ---------------------------------------------------------------- start


def read_prompt(args) -> str | None:
    if args.prompt_file:
        text = Path(args.prompt_file).read_text()
    elif args.prompt == "-":
        text = sys.stdin.read()
    else:
        text = args.prompt
    if text is None:
        return None
    if not (text := text.strip()):
        die("the prompt is empty")
    if len(text.encode()) > MAX_PROMPT_BYTES:
        die(f"the prompt is over {MAX_PROMPT_BYTES} bytes; put the detail in a file and point the prompt at it")
    return text


def check_command(command: list[str], prompt: str | None):
    if not command:
        die("give the agent command after --, e.g. -- claude --name {name} -- {prompt}")
    uses_prompt = any("{prompt}" in a for a in command)
    if prompt and not uses_prompt:
        die("a prompt was given but the command has no {prompt} placeholder")
    if uses_prompt and not prompt:
        die("the command has a {prompt} placeholder but no prompt was given")


def start(rec: dict, wait: float) -> int:
    """Start the tmux session for `rec` and wait for the agent to come up."""
    name = rec["name"]
    if exists(name):
        die(f"session {name} already exists; attach with: tmux attach -t {name}")
    if not Path(rec["dir"]).is_dir():
        die(f"no such directory: {rec['dir']}")
    if not shutil.which(rec["command"][0]):
        die(f"{rec['command'][0]} is not on PATH")
    # A fresh ID for a new conversation; a resume passes its known one.
    if not rec.get("session_id") or not rec.get("keep_session_id"):
        rec["session_id"] = str(uuid.uuid4()) if any("{session_id}" in a for a in rec["command"]) else None
    rec["launched_at"] = int(time.time())
    for key in ("ended", "closed_at"):
        rec.pop(key, None)
    save_record(rec)
    tmux(
        "new-session", "-d", "-s", name, "-c", rec["dir"],
        sys.executable, str(Path(__file__).resolve()), "_exec", str(record_path(name)),
    )  # fmt: skip
    # The harnesses' tmux-state hooks set @agent_state once the agent is up
    # (see harnesses/tmux-state.nix); other agents just get the wait.
    deadline = time.monotonic() + wait
    while time.monotonic() < deadline:
        time.sleep(0.25)
        if (load_record(name) or {}).get("ended") or not exists(name):
            break
        if option(name, "@agent_state") in STATES:
            break
    return report_start(name)


def report_start(name: str) -> int:
    rec = load_record(name) or {}
    ended = rec.get("ended")
    if not exists(name):
        print(f"{name}: exited during startup" + (f" with status {ended['status']}" if ended else ""))
        return 1
    tail = capture(name, 8)
    if ended:
        print(f"{name}: {rec['command'][0]} exited with status {ended['status']}; the pane is kept for inspection")
        print("\n".join(f"  | {ln}" for ln in tail))
        return 1
    state = option(name, "@agent_state") or "unknown"
    print(f"{name}: started in {rec['dir']}, state {state}")
    print("\n".join(f"  | {ln}" for ln in tail))
    if dialog_open(name):
        print(
            f"{name} is showing a dialog (above), such as a folder-trust or hook-review check; "
            "the kickoff prompt waits behind it. Ask the user to attach and answer it; "
            "`send` refuses while it is open."
        )
    if rec.get("session_id"):
        print(f"session id {rec['session_id']}")
    print(f"attach: tmux attach -t {name}")
    return 0


def cmd_launch(args) -> int:
    if not NAME_RE.match(args.name):
        die(f"invalid session name {args.name!r}: use letters, digits, '-' and '_' (at most 48)")
    prompt = read_prompt(args)
    check_command(args.command, prompt)
    if args.base and not args.worktree:
        die("--base needs --worktree")
    if exists(args.name):  # before making a worktree for it
        die(f"session {args.name} already exists; attach with: tmux attach -t {args.name}")
    directory = worktree(args.dir, args.worktree, args.base) if args.worktree else args.dir
    rec = {"name": args.name, "dir": str(Path(directory).resolve()), "command": args.command, "prompt": prompt}
    if args.session_id:
        rec.update(session_id=args.session_id, keep_session_id=True)
    if args.worktree:
        rec["worktree"] = {"repo": str(Path(args.dir).resolve()), "branch": args.worktree}
    check_shared_checkout(args.name, rec["dir"], args.share)
    status = start(rec, args.wait)
    return status or (attach(args.name) if args.attach else 0)


def cmd_relaunch(args) -> int:
    rec = load_record(args.name)
    if not rec or not rec.get("command"):
        die(f"no launch record for {args.name}")
    status = git_status(rec["dir"])
    if status and not args.force:
        die(f"{rec['dir']} has uncommitted changes; pass --force to relaunch anyway:\n{status}")
    if exists(args.name):
        close(args.name, grace=0)
    rec.pop("keep_session_id", None)  # a relaunch starts a new conversation
    return start(rec, args.wait)


def attach(name: str) -> int:
    verb = "switch-client" if os.environ.get("TMUX") else "attach-session"
    os.execvp("tmux", ["tmux", verb, "-t", f"={name}"])


def cmd_exec(args) -> int:
    """Pane process: run the agent for a launch record and keep failures visible."""
    # The pane has the tmux server's environment, not the launcher's: keep
    # using the state directory that holds this record.
    os.environ["COORDINATOR_STATE_DIR"] = str(Path(args.record).resolve().parent.parent)
    rec = json.loads(Path(args.record).read_text())
    argv = expand(rec)
    env = {k: v for k, v in os.environ.items() if k not in NESTING_ENV}
    try:
        status = subprocess.run(argv, env=env).returncode
    except FileNotFoundError:
        print(f"coordinator: {argv[0]} not found on PATH", file=sys.stderr)
        status = 127
    except KeyboardInterrupt:
        status = 130
    rec = load_record(rec["name"]) or rec
    rec["ended"] = {"status": status, "at": int(time.time())}
    save_record(rec)
    if status:
        print(f"\n[{argv[0]} exited with status {status}; press Enter to close this session]")
        try:
            input()
        except (EOFError, KeyboardInterrupt):
            pass
    return status


# ---------------------------------------------------------------- inspect


def age(seconds: float) -> str:
    seconds = max(0, int(seconds))
    for unit, size in (("d", 86400), ("h", 3600), ("m", 60)):
        if seconds >= size:
            return f"{seconds // size}{unit}"
    return f"{seconds}s"


def roster() -> list[dict]:
    now = time.time()
    me = current_session()
    rows = []
    for s in sessions():
        rec = load_record(s["session_name"]) or {}
        state = s["@agent_state"]
        if (ended := rec.get("ended")) and ended["status"]:
            state = f"exited({ended['status']})"
        brief = (rec.get("prompt") or "").splitlines()
        rows.append(
            {
                "name": s["session_name"],
                "self": s["session_name"] == me,
                "agent": rec["command"][0] if rec.get("command") else "",
                "state": state or "-",
                "attached": int(s["session_attached"] or 0) > 0,
                "age": age(now - int(s["session_created"] or now)),
                "idle": age(now - int(s["session_activity"] or now)),
                "dir": s["pane_current_path"],
                "managed": bool(rec),
                "session_id": rec.get("session_id"),
                "brief": brief[0][:80] if brief else "",
            }
        )
    return rows


def cmd_list(args) -> int:
    rows = roster()
    if args.json:
        print(json.dumps(rows, indent=2))
        return 0
    if not rows:
        print("no tmux sessions")
        return 0
    home = str(Path.home())
    table = [("NAME", "AGENT", "STATE", "ATT", "AGE", "IDLE", "DIR", "BRIEF")]
    for r in rows:
        d = "~" + r["dir"][len(home):] if r["dir"].startswith(home) else r["dir"]
        brief = r["brief"] if r["managed"] else "(not launched by this helper)"
        attached = "yes" if r["attached"] else ""
        table.append((r["name"] + (" *" if r["self"] else ""), r["agent"] or "-", r["state"], attached,
                      r["age"], r["idle"], d, brief))  # fmt: skip
    widths = [max(len(row[i]) for row in table) for i in range(7)]
    for row in table:
        print("  ".join(c.ljust(w) for c, w in zip(row, widths)) + "  " + row[7])
    return 0


def cmd_show(args) -> int:
    rec = load_record(args.name)
    if not rec:
        die(f"no launch record for {args.name}")
    if args.field:
        print(rec.get(args.field) or "")
    else:
        print(json.dumps(rec, indent=2))
    return 0


def require_session(name: str):
    if exists(name):
        return
    rec = load_record(name) or {}
    if ended := rec.get("ended"):
        when = time.strftime("%H:%M:%S", time.localtime(ended["at"]))
        die(f"no session {name}: its agent exited with status {ended['status']} at {when}")
    if rec.get("closed_at"):
        die(f"no session {name}: it was closed at {time.strftime('%H:%M:%S', time.localtime(rec['closed_at']))}")
    die(f"no session {name}")


def cmd_peek(args) -> int:
    require_session(args.name)
    print(f"{args.name}: state {option(args.name, '@agent_state') or 'unknown'}")
    print("\n".join(capture(args.name, args.lines)))
    return 0


def cmd_send(args) -> int:
    require_session(args.name)
    text = Path(args.file).read_text() if args.file else sys.stdin.read() if args.text == "-" else args.text
    if not (text := (text or "").strip()):
        die("nothing to send")
    state = option(args.name, "@agent_state")
    if (state == "input" or dialog_open(args.name)) and not args.force:
        die(
            f"{args.name} is waiting on a dialog, permission prompt or question; "
            "typed text could answer it. Look with `peek`, then answer it in the session or pass --force."
        )
    if state == "working":
        print(f"{args.name} is mid-turn: the agent takes the message when it can")
    # A bracketed paste keeps newlines from submitting early, and agent TUIs
    # insert it as text (Claude Code even in vim normal mode). Enter goes
    # separately after a pause: Codex reads an Enter inside a burst of input
    # as a newline.
    buf = f"coordinator-{args.name}"
    tmux("load-buffer", "-b", buf, "-", input=text)
    tmux("paste-buffer", "-p", "-d", "-b", buf, "-t", pane(args.name))
    time.sleep(args.delay)
    tmux("send-keys", "-t", pane(args.name), "Enter")
    print(f"sent {len(text)} characters to {args.name}")
    return 0


def descendants(pids: set[int]) -> set[int]:
    """`pids` and every process below them (Linux /proc; just `pids` elsewhere)."""
    children: dict[int, list[int]] = {}
    for d in Path("/proc").glob("[0-9]*"):
        try:
            ppid = int((d / "stat").read_text().rsplit(")", 1)[1].split()[1])
        except (OSError, ValueError, IndexError):
            continue
        children.setdefault(ppid, []).append(int(d.name))
    out, todo = set(), list(pids)
    while todo:
        if (pid := todo.pop()) not in out:
            out.add(pid)
            todo += children.get(pid, [])
    return out


def close(name: str, grace: float):
    directory = option(name, "pane_current_path")
    panes, pids = set(), set()
    for line in tmux("list-panes", "-s", "-t", f"={name}", "-F", "#{pane_id} #{pane_pid}").splitlines():
        pane_id, pid = line.split()
        panes.add(pane_id)
        pids.add(int(pid))
    procs = descendants(pids)
    tmux("kill-session", "-t", f"={name}")
    # Return only once the agent has exited, so that a resume right after
    # never runs alongside the old process on the same conversation.
    deadline = time.monotonic() + 10
    while (alive := {p for p in procs if Path(f"/proc/{p}").exists()}) and time.monotonic() < deadline:
        time.sleep(0.1)
    print(f"closed {name}")
    if alive:
        print(f"warning: {len(alive)} process(es) of {name} still running 10s after the hangup")
    if rec := load_record(name):
        rec["closed_at"] = int(time.time())
        save_record(rec)
    if directory and (status := git_status(directory)) is not None:
        print(f"{directory}: " + ("clean" if not status else f"uncommitted changes:\n{status}"))
    time.sleep(grace)
    if strays := stray_processes(panes):
        print("still running from the closed session (not killed; stop them if they are not wanted):")
        print_procs(strays)


def cmd_close(args) -> int:
    require_session(args.name)
    close(args.name, args.grace)
    return 0


def cmd_strays(args) -> int:
    if not (strays := stray_processes()):
        print("no processes left over from closed tmux panes")
        return 0
    print("processes left over from closed tmux panes:")
    print_procs(strays)
    return 0


# ---------------------------------------------------------------- cli


def main(argv=None) -> int:
    p = argparse.ArgumentParser(
        prog="coordinator.py", description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter
    )
    sub = p.add_subparsers(dest="cmd", required=True)

    s = sub.add_parser("launch", help="start an agent in a new tmux session: launch NAME [options] -- COMMAND...")
    s.add_argument("name")
    s.add_argument("--dir", required=True, help="working directory, or with --worktree the repository")
    s.add_argument("--worktree", metavar="BRANCH", help="run in a worktree of --dir for BRANCH (or pr:N), made if needed")
    s.add_argument("--base", help="base ref for a new --worktree branch (default: the default branch)")
    s.add_argument("--share", action="store_true", help="allow a Git checkout another live session uses")
    g = s.add_mutually_exclusive_group()
    g.add_argument("--prompt", help="the brief for {prompt}, or - to read it from stdin")
    g.add_argument("--prompt-file", help="read the brief for {prompt} from a file")
    s.add_argument("--wait", type=float, default=15.0, help="seconds to wait for startup (default %(default)s)")
    s.add_argument("--attach", action="store_true", help="attach once started")
    s.add_argument("--session-id", help="a known conversation ID for {session_id}, e.g. to resume it")
    s.set_defaults(fn=cmd_launch)

    s = sub.add_parser("relaunch", help="restart a session from scratch with its recorded command and brief")
    s.add_argument("name")
    s.add_argument("--force", action="store_true", help="relaunch even if the directory has changes")
    s.add_argument("--wait", type=float, default=15.0)
    s.set_defaults(fn=cmd_relaunch)

    s = sub.add_parser("send", help="type a message into a session and submit it")
    s.add_argument("name")
    s.add_argument("text", nargs="?", help="message text, or - to read it from stdin")
    s.add_argument("--file", help="read the message from a file")
    s.add_argument("--force", action="store_true", help="send even if the session waits on a prompt")
    s.add_argument("--delay", type=float, default=0.5, help=argparse.SUPPRESS)
    s.set_defaults(fn=cmd_send)

    s = sub.add_parser("list", help="list tmux sessions with agent, state and brief")
    s.add_argument("--json", action="store_true")
    s.set_defaults(fn=cmd_list)

    s = sub.add_parser("show", help="print a session's launch record (command, brief, session id)")
    s.add_argument("name")
    s.add_argument("--field", help="print only this field, e.g. session_id")
    s.set_defaults(fn=cmd_show)

    s = sub.add_parser("peek", help="show the last lines of a session's screen")
    s.add_argument("name")
    s.add_argument("-n", "--lines", type=int, default=20)
    s.set_defaults(fn=cmd_peek)

    s = sub.add_parser("close", help="kill a session and report leftovers")
    s.add_argument("name")
    s.add_argument("--grace", type=float, default=1.0, help=argparse.SUPPRESS)
    s.set_defaults(fn=cmd_close)

    s = sub.add_parser("strays", help="list processes left over from closed tmux panes")
    s.set_defaults(fn=cmd_strays)

    s = sub.add_parser("_exec")
    s.add_argument("record")
    s.set_defaults(fn=cmd_exec)

    # Everything after the first -- is the agent's command line, which may
    # have its own --.
    argv = sys.argv[1:] if argv is None else argv
    command = argv[argv.index("--") + 1 :] if "--" in argv else []
    args = p.parse_args(argv[: len(argv) - len(command) - 1] if "--" in argv else argv)
    args.command = command
    if args.cmd != "_exec" and not shutil.which("tmux"):
        die("tmux is not on PATH")
    return args.fn(args) or 0


if __name__ == "__main__":
    sys.exit(main())
