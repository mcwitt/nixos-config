"""Publish an Antigravity CLI session's state as the tmux pane option
@agent_state, from the status reports (OSC 7501) the CLI writes to its
terminal. Run by `tmux pipe-pane` with the pane's output on stdin; see
./tmux-state.nix.

usage: agy-tmux-state <agent-tmux-state> <tmux> <socket> <pane> <pid of agy>
"""

import os
import re
import select
import subprocess
import sys
import time

REPORT = re.compile(rb"\x1b\]7501;([^\x07\x1b]*)(?:\x07|\x1b\\)")
START = b"\x1b]7501;"
# `idle` is the prompt before the first turn, and after a key press there.
STATES = {"working": "working", "blocked": "input", "done": "done", "idle": "done"}
# The CLI reports `blocked:kind=auth` for a moment while it loads its sign-in
# at startup, so that kind counts only once it has lasted this long.
AUTH_DELAY = 1.0
POLL = 0.2
# The pane option that names the `agy` whose watcher owns the pane's state.
OWNER = "@agy_state_pid"


def main():
    publisher, tmux, socket, pane, pid = sys.argv[1:6]
    pid = int(pid)
    # pipe-pane commands do not inherit these from the pane.
    env = dict(os.environ, TMUX=f"{socket},0,0", TMUX_PANE=pane)
    published = None

    def publish(state):
        nonlocal published
        if state != published:
            subprocess.run([publisher, state], env=env, stdin=subprocess.DEVNULL)
            published = state

    def alive():
        try:
            os.kill(pid, 0)
        except ProcessLookupError:
            return False
        except PermissionError:
            pass
        return True

    buf = b""
    block_at = None
    while alive():
        now = time.monotonic()
        if block_at is not None and now >= block_at:
            publish("input")
            block_at = None
        wait = POLL if block_at is None else min(POLL, block_at - now)
        if not select.select([0], [], [], wait)[0]:
            continue
        data = os.read(0, 65536)
        if not data:
            # The pane closed, or another pipe-pane replaced this one.
            break
        buf += data
        end = 0
        for report in REPORT.finditer(buf):
            end = report.end()
            fields = dict(
                field.split("=", 1)
                for field in report.group(1).decode("ascii", "replace").split(":")
                if "=" in field
            )
            # The pane's other programs may write reports of their own.
            if fields.get("app") != "antigravity":
                continue
            state = STATES.get(fields.get("state"))
            if state == "input" and fields.get("kind") == "auth":
                if block_at is None and published != "input":
                    block_at = time.monotonic() + AUTH_DELAY
            elif state:
                block_at = None
                publish(state)
        # Keep a report that is still arriving, or a split start marker.
        buf = buf[end:]
        start = buf.rfind(START)
        buf = buf[start:] if start >= 0 else buf[-len(START) :]
        if len(buf) > 1 << 20:
            buf = b""

    # Clean up only what is still this watcher's, in one tmux command so that
    # nothing changes between the checks and the changes: a later `agy` in the
    # pane takes the option over, and another program may have taken the pipe.
    mine = f"#{{==:#{{{OWNER}}},{pid}}}"
    my_pipe = f"#{{==:#{{pane_pipe_pid}},{os.getpid()}}}"
    clear = (
        f"set -pu -t {pane} {OWNER} ; set -pu -t {pane} @agent_state ; "
        f"if -t {pane} -F '{my_pipe}' 'pipe-pane -t {pane}'"
    )
    subprocess.run(
        [tmux, "-S", socket, "if", "-t", pane, "-F", mine, clear],
        stdin=subprocess.DEVNULL,
        stderr=subprocess.DEVNULL,
    )


if __name__ == "__main__":
    main()
