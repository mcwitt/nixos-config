"""Tests for the watcher that publishes Antigravity's terminal status reports.

Usage: python3 test_agy_tmux_state.py <path/to/antigravity-cli/tmux-state.py>
"""

import os
import subprocess
import sys
import tempfile
import time
import unittest
from pathlib import Path

SCRIPT = Path(sys.argv.pop(1)).resolve() if len(sys.argv) > 1 else None


def report(state, **fields):
    body = ":".join([f"state={state}", *(f"{k}={v}" for k, v in fields.items())])
    return f"\x1b]7501;{body}:app=antigravity\x07".encode()


class Watcher(unittest.TestCase):
    def setUp(self):
        self.dir = Path(tempfile.mkdtemp())
        self.log = self.dir / "states"
        self.log.touch()
        # Stands for both agent-tmux-state and tmux: logs its first argument.
        publisher = self.dir / "publish"
        publisher.write_text(f'#!/bin/sh\necho "$1" >> {self.log}\n')
        publisher.chmod(0o755)
        self.agent = subprocess.Popen(["sleep", "60"])
        self.watcher = subprocess.Popen(
            [sys.executable, SCRIPT, publisher, publisher, "socket", "%1", str(self.agent.pid)],
            stdin=subprocess.PIPE,
        )

    def tearDown(self):
        for p in (self.watcher, self.agent):
            p.kill()
            p.wait()

    def write(self, data):
        self.watcher.stdin.write(data)
        self.watcher.stdin.flush()

    def states(self):
        return self.log.read_text().split()

    def expect(self, states):
        deadline = time.monotonic() + 10
        while self.states() != states and time.monotonic() < deadline:
            time.sleep(0.05)
        self.assertEqual(self.states(), states)

    def test_turn(self):
        self.write(b"text" + report("idle") + b"more")
        self.expect(["done"])
        self.write(report("working"))
        self.expect(["done", "working"])
        self.write(report("done", msg="b2s=") + report("idle"))
        self.expect(["done", "working", "done"])

    def test_report_split_across_reads(self):
        data = report("working")
        for part in (data[:3], data[3:12], data[12:]):
            self.write(part)
            time.sleep(0.1)
        self.expect(["working"])

    def test_block_must_last(self):
        self.write(report("blocked", kind="auth") + report("idle"))
        time.sleep(1.5)
        self.expect(["done"])
        self.write(report("working") + report("blocked", kind="permission", msg="eA=="))
        self.expect(["done", "working", "input"])
        self.write(report("working"))
        self.expect(["done", "working", "input", "working"])

    def test_clears_when_the_agent_exits(self):
        self.write(report("working"))
        self.expect(["working"])
        self.agent.kill()
        self.agent.wait()
        # "off" to agent-tmux-state, then "-S" to tmux, to close the pipe.
        self.expect(["working", "off", "-S"])
        self.assertEqual(self.watcher.wait(timeout=5), 0)

    def test_clears_when_the_pipe_closes(self):
        self.write(report("working"))
        self.expect(["working"])
        self.watcher.stdin.close()
        self.expect(["working", "off"])


if __name__ == "__main__":
    if SCRIPT is None:
        sys.exit(__doc__)
    unittest.main()
