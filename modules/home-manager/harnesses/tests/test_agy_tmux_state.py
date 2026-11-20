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


def report(state, app="antigravity", **fields):
    body = ":".join([f"state={state}", *(f"{k}={v}" for k, v in fields.items())])
    return f"\x1b]7501;{body}:app={app}\x07".encode()


class Watcher(unittest.TestCase):
    def setUp(self):
        tmp = tempfile.TemporaryDirectory()
        self.addCleanup(tmp.cleanup)
        self.dir = Path(tmp.name)
        self.log = self.dir / "states"
        self.tmux_log = self.dir / "commands"
        for name, log in (("publish", self.log), ("tmux", self.tmux_log)):
            log.touch()
            script = self.dir / name
            script.write_text(f'#!/bin/sh\necho "$@" >> {log}\n')
            script.chmod(0o755)
        self.agent = subprocess.Popen(["sleep", "60"])
        self.watcher = subprocess.Popen(
            [
                sys.executable,
                SCRIPT,
                self.dir / "publish",
                self.dir / "tmux",
                "socket",
                "%1",
                str(self.agent.pid),
            ],
            stdin=subprocess.PIPE,
        )

    def tearDown(self):
        if not self.watcher.stdin.closed:
            self.watcher.stdin.close()
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

    def expect_cleanup(self):
        """One tmux command, conditional on this watcher still owning the pane."""
        self.assertEqual(self.watcher.wait(timeout=5), 0)
        lines = self.tmux_log.read_text().splitlines()
        self.assertEqual(len(lines), 1, lines)
        command = lines[0]
        owner = f"#{{==:#{{@agent_state_owner}},{self.agent.pid}}}"
        pipe = f"#{{==:#{{pane_pipe_pid}},{self.watcher.pid}}}"
        self.assertEqual(
            command,
            f"-S socket if -t %1 -F {owner} set -pu -t %1 @agent_state_owner ; "
            f"set -pu -t %1 @agent_state ; if -t %1 -F {pipe} pipe-pane -t %1",
        )

    def test_turn(self):
        self.write(b"text" + report("idle") + b"more")
        self.expect(["done"])
        self.write(report("working"))
        self.expect(["done", "working"])
        self.write(report("done", msg="b2s=") + report("idle"))
        self.expect(["done", "working", "done"])

    def test_drops_a_report_made_while_exiting(self):
        self.write(report("working"))
        self.expect(["working"])
        self.write(report("idle"))
        self.agent.kill()
        self.agent.wait()
        self.expect_cleanup()
        self.assertEqual(self.states(), ["working"])

    def test_report_split_across_reads(self):
        data = report("working")
        for part in (data[:3], data[3:12], data[12:]):
            self.write(part)
            time.sleep(0.1)
        self.expect(["working"])

    def test_sign_in_block_must_last(self):
        self.write(report("blocked", kind="auth") + report("idle"))
        time.sleep(1.5)
        self.expect(["done"])
        self.write(report("blocked", kind="auth"))
        time.sleep(0.5)
        self.expect(["done"])
        self.expect(["done", "input"])

    def test_other_blocks_are_prompt(self):
        self.write(report("working"))
        self.expect(["working"])
        self.write(report("blocked", kind="permission", msg="eA=="))
        time.sleep(0.5)
        self.assertEqual(self.states(), ["working", "input"])
        self.write(report("working"))
        self.expect(["working", "input", "working"])

    def test_ignores_other_programs(self):
        self.write(report("working") + report("blocked", app="other") + b"\x1b]7501;state=done\x07")
        time.sleep(0.5)
        self.expect(["working"])

    def test_cleans_up_when_the_agent_exits(self):
        self.write(report("working"))
        self.expect(["working"])
        self.agent.kill()
        self.agent.wait()
        self.expect_cleanup()

    def test_cleans_up_when_the_pipe_closes(self):
        self.write(report("working"))
        self.expect(["working"])
        self.watcher.stdin.close()
        self.expect_cleanup()


if __name__ == "__main__":
    if SCRIPT is None:
        sys.exit(__doc__)
    unittest.main()
