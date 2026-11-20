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
# The CLI reports `blocked` for a moment while it loads its sign-in at
# startup, so a block counts only once it has lasted this long.
BLOCK_DELAY = 1.0
POLL = 1.0


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
            state = STATES.get(fields.get("state"))
            if state == "input":
                if block_at is None and published != "input":
                    block_at = time.monotonic() + BLOCK_DELAY
            elif state:
                block_at = None
                publish(state)
        # Keep a report that is still arriving, or a split start marker.
        buf = buf[end:]
        start = buf.rfind(START)
        buf = buf[start:] if start >= 0 else buf[-len(START) :]
        if len(buf) > 1 << 20:
            buf = b""

    publish("off")
    if not alive():
        subprocess.run(
            [tmux, "-S", socket, "pipe-pane", "-t", pane],
            stdin=subprocess.DEVNULL,
            stderr=subprocess.DEVNULL,
        )


if __name__ == "__main__":
    main()
