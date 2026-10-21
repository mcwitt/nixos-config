"""Tests for the coordinator skill's session helper, on a private tmux server with a stub agent.

Usage: python3 test_coordinator.py <path/to/coordinator.py>
"""

import json
import os
import shutil
import signal
import subprocess
import sys
import tempfile
import time
import unittest
from pathlib import Path

SCRIPT = Path(sys.argv.pop(1)).resolve() if len(sys.argv) > 1 else None

# Installed as `claude` and as `agent`. Records its argv, cwd and environment;
# as `claude` it reports a state the way the Claude Code tmux-state hooks do.
# Then it logs every line typed into the pane.
STUB = f"""#!{sys.executable}
import json, os, subprocess, sys, time
args = sys.argv[1:]
log = os.path.join(os.environ["STUB_LOG"], os.environ["TMUX_PANE"].lstrip("%"))
prompt = args[-1] if args else ""
with open(log + ".json", "w") as f:
    json.dump({{"argv": args, "cwd": os.getcwd(), "env": dict(os.environ)}}, f)
if "EXIT 3" in prompt:
    sys.exit(3)
if "EXIT 0" in prompt:
    sys.exit(0)
if "SPAWN" in prompt:
    p = subprocess.Popen(["sleep", "300"], start_new_session=True)
    open(log + ".spawned", "w").write(str(p.pid))
if "DIALOG" in prompt:
    print(" Yes, I trust this folder\\n Enter to confirm · Esc to cancel", flush=True)
    for line in sys.stdin:
        with open(log + ".typed", "a") as f:
            f.write(line)
time.sleep(0.3)
if os.path.basename(sys.argv[0]) == "claude":
    subprocess.run(["tmux", "set", "-p", "-t", os.environ["TMUX_PANE"], "@agent_state", "done"])
sys.stdout.write("\\x1b[?2004h> ")
sys.stdout.flush()
for line in sys.stdin:
    with open(log + ".typed", "a") as f:
        f.write(line)
"""
CLAUDE = ["claude", "--name", "{name}", "--session-id", "{session_id}", "--", "{prompt}"]


class CoordinatorTest(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.tmp = Path(tempfile.mkdtemp())
        t = cls.tmp
        for d in ("bin", "log", "home"):
            (t / d).mkdir()
        for name in ("claude", "agent"):
            (t / "bin" / name).write_text(STUB)
            (t / "bin" / name).chmod(0o755)
        server_env = {k: v for k, v in os.environ.items() if k not in ("TMUX", "TMUX_PANE", "XDG_STATE_HOME")} | {
            "PATH": f"{t / 'bin'}:{os.environ['PATH']}",
            "HOME": str(t / "home"),
            "XDG_CONFIG_HOME": str(t / "home" / ".config"),
            "TMUX_TMPDIR": str(t),
            "STUB_LOG": str(t / "log"),
            "CLAUDECODE": "1",
            "GIT_CONFIG_NOSYSTEM": "1",
            "GIT_CONFIG_GLOBAL": str(t / "gitconfig"),
            "TERM": "xterm-256color",
        }
        # The server, and so every pane, lacks the launcher's state directory:
        # the pane process must find it from the record path.
        cls.env = server_env
        cls.tmux("new-session", "-d", "-s", "keepalive", "-x", "200", "-y", "50")
        cls.env = server_env | {"COORDINATOR_STATE_DIR": str(t / "state")}
        repo = cls.repo = t / "repo"
        repo.mkdir()
        for args in (("init", "-q"), ("commit", "-q", "--allow-empty", "-m", "init")):
            subprocess.run(
                ["git", "-c", "user.name=t", "-c", "user.email=t@example.com", *args],
                cwd=repo, env=cls.env, check=True,
            )  # fmt: skip
        cls.plain = t / "plain"
        cls.plain.mkdir()

    @classmethod
    def tearDownClass(cls):
        cls.tmux("kill-server", check=False)
        for f in (cls.tmp / "log").glob("*.spawned"):
            try:
                os.kill(int(f.read_text()), signal.SIGKILL)
            except (OSError, ValueError):
                pass
        shutil.rmtree(cls.tmp, ignore_errors=True)

    @classmethod
    def tmux(cls, *args, check=True):
        r = subprocess.run(["tmux", *args], env=cls.env, capture_output=True, text=True)
        if check and r.returncode:
            raise AssertionError(f"tmux {args}: {r.stderr}")
        return r.stdout.strip()

    def run_script(self, *args, input=None, ok=True, env=None):
        r = subprocess.run(
            [sys.executable, str(SCRIPT), *args],
            env=env or self.env, input=input, capture_output=True, text=True, timeout=60,
        )  # fmt: skip
        if ok is not None and (r.returncode == 0) != ok:
            self.fail(f"{args} exited {r.returncode}\nstdout: {r.stdout}\nstderr: {r.stderr}")
        return r

    def launch(self, name, *opts, command=CLAUDE, prompt="go", ok=True):
        extra = ("--prompt", prompt) if prompt is not None else ()
        return self.run_script("launch", name, "--dir", str(self.plain), *extra, *opts, "--", *command, ok=ok)

    def close(self, name):
        self.run_script("close", name, "--grace", "0")

    def stub_log(self, name, suffix=".json", timeout=10):
        pane_id = self.tmux("display-message", "-p", "-t", f"={name}:", "#{pane_id}").lstrip("%")
        path = self.tmp / "log" / f"{pane_id}{suffix}"
        self.wait_for(path.exists, timeout)
        return path

    def argv(self, name):
        return json.loads(self.stub_log(name).read_text())["argv"]

    def record(self, name):
        return json.loads((self.tmp / "state" / "sessions" / f"{name}.json").read_text())

    def names(self):
        return self.tmux("list-sessions", "-F", "#{session_name}").split()

    def wait_for(self, cond, timeout=10):
        deadline = time.monotonic() + timeout
        while time.monotonic() < deadline:
            if cond():
                return True
            time.sleep(0.1)
        return False

    def test_launch_fills_placeholders_verbatim(self):
        brief = "-leading dash\nIt's \"quoted\", $(not run) and `ticks` $HOME; {name}\n\ttabbed"
        r = self.run_script("launch", "brief", "--dir", str(self.plain), "--prompt", "-", "--", *CLAUDE, input=brief)
        self.assertIn("state done", r.stdout)
        self.assertIn("tmux attach -t brief", r.stdout)
        rec = self.record("brief")
        got = json.loads(self.stub_log("brief").read_text())
        self.assertEqual(got["argv"], ["--name", "brief", "--session-id", rec["session_id"], "--", brief])
        self.assertIn(f"session id {rec['session_id']}", r.stdout)
        self.assertEqual(got["cwd"], str(self.plain))
        self.assertNotIn("CLAUDECODE", got["env"])
        self.assertEqual(rec["command"], CLAUDE)
        self.assertEqual(json.loads(self.run_script("show", "brief").stdout)["prompt"], brief)

        rows = {r["name"]: r for r in json.loads(self.run_script("list", "--json").stdout)}
        self.assertEqual(rows["brief"]["state"], "done")
        self.assertEqual(rows["brief"]["agent"], "claude")
        self.assertEqual(rows["brief"]["brief"], "-leading dash")
        self.assertFalse(rows["keepalive"]["managed"])
        self.assertIn("(not launched by this helper)", self.run_script("list").stdout)

        self.close("brief")
        self.assertNotIn("brief", self.names())
        r = self.run_script("peek", "brief", ok=False)
        self.assertIn("it was closed at", r.stderr)

    def test_any_agent_command(self):
        # No state hooks: the launch just waits, and a resume needs no prompt.
        self.launch("other", "--wait", "1", command=["agent", "--prompt-interactive={prompt}"], prompt="hi there")
        self.assertEqual(self.argv("other"), ["--prompt-interactive=hi there"])
        self.assertIsNone(self.record("other")["session_id"])
        self.close("other")
        self.launch("other", "--wait", "1", command=["agent", "resume", "--last"], prompt=None)
        self.assertEqual(self.argv("other"), ["resume", "--last"])
        self.close("other")
        # A resume passes the recorded ID; it is kept, not replaced.
        self.launch("other", "--wait", "1", "--session-id", "abc-123", command=["agent", "--resume", "{session_id}"],
                    prompt=None)  # fmt: skip
        self.assertEqual(self.argv("other"), ["--resume", "abc-123"])
        self.assertEqual(self.record("other")["session_id"], "abc-123")
        self.close("other")

    def test_rejects_bad_launches(self):
        r = self.launch("bad.name", ok=False)
        self.assertIn("invalid session name", r.stderr)
        r = self.launch("nocmd", command=[], ok=False)
        self.assertIn("give the agent command", r.stderr)
        r = self.launch("noslot", command=["claude"], ok=False)
        self.assertIn("no {prompt} placeholder", r.stderr)
        r = self.launch("noprompt", prompt=None, ok=False)
        self.assertIn("no prompt was given", r.stderr)
        r = self.launch("missing", command=["no-such-agent", "{prompt}"], ok=False)
        self.assertIn("not on PATH", r.stderr)
        self.launch("dup")
        r = self.launch("dup", ok=False)
        self.assertIn("already exists", r.stderr)
        self.close("dup")

    def test_shared_checkout_needs_share(self):
        def run(name, *opts, ok=True):
            return self.run_script("launch", name, "--dir", str(self.repo), "--prompt", "x", *opts, "--", *CLAUDE, ok=ok)

        run("edit-a")
        r = run("edit-b", ok=False)
        self.assertIn("edit-a", r.stderr)
        self.assertIn("worktree", r.stderr)
        run("edit-b", "--share")
        self.assertIn("clean", self.run_script("close", "edit-b", "--grace", "0").stdout)
        self.close("edit-a")

    def check_worktree(self, name, branch, wt):
        env = self.env | {"COORDINATOR_WT": wt}
        for prompt in ("edit things", "again"):  # the second reuses the worktree
            self.run_script("launch", name, "--dir", str(self.repo), "--worktree", branch, "--prompt", prompt,
                            "--", *CLAUDE, env=env)  # fmt: skip
            cwd = Path(json.loads(self.stub_log(name).read_text())["cwd"])
            self.close(name)
        self.assertNotEqual(cwd, self.repo)
        head = subprocess.run(["git", "-C", cwd, "branch", "--show-current"], capture_output=True, text=True)
        self.assertEqual(head.stdout.strip(), branch)
        self.assertEqual(self.record(name)["worktree"], {"repo": str(self.repo), "branch": branch})
        return cwd

    def test_worktree_with_git(self):
        self.assertEqual(self.check_worktree("wt-git", "feature/git", ""), self.tmp / "repo.feature-git")

    @unittest.skipUnless(shutil.which("wt"), "needs worktrunk")
    def test_worktree_with_worktrunk(self):
        self.check_worktree("wt-wt", "feature-wt", "wt")

    def test_send(self):
        self.launch("chat")
        self.run_script("send", "chat", "first line\nsecond 'line' $(x)", "--delay", "0.2")
        typed = self.stub_log("chat", ".typed")
        self.assertTrue(self.wait_for(lambda: typed.exists() and "second" in typed.read_text()))
        text = typed.read_text()
        self.assertIn("first line", text)
        self.assertIn("second 'line' $(x)", text)
        self.assertIn("\x1b[200~", text)  # sent as a bracketed paste

        self.tmux("set", "-p", "-t", "=chat:", "@agent_state", "input")
        r = self.run_script("send", "chat", "yes", ok=False)
        self.assertIn("waiting on a dialog", r.stderr)
        self.run_script("send", "chat", "forced", "--force", "--delay", "0.2")
        self.assertTrue(self.wait_for(lambda: "forced" in typed.read_text()))

        self.tmux("set", "-p", "-t", "=chat:", "@agent_state", "working")
        self.assertIn("mid-turn", self.run_script("send", "chat", "later", "--delay", "0.2").stdout)
        self.assertIn("state working", self.run_script("peek", "chat", "-n", "5").stdout)
        self.close("chat")
        self.run_script("send", "chat", "gone", ok=False)

    def test_dialog_blocks_send(self):
        r = self.launch("trust", "--wait", "2", prompt="DIALOG")
        self.assertIn("state unknown", r.stdout)
        self.assertIn("showing a dialog", r.stdout)
        r = self.run_script("send", "trust", "hi", ok=False)
        self.assertIn("waiting on a dialog", r.stderr)
        self.assertFalse(self.stub_log("trust", ".typed", timeout=1).exists())
        self.close("trust")

    def test_exits_are_recorded(self):
        r = self.launch("crash", prompt="EXIT 3", ok=False)
        self.assertIn("exited with status 3", r.stdout)
        rows = {r["name"]: r for r in json.loads(self.run_script("list", "--json").stdout)}
        self.assertEqual(rows["crash"]["state"], "exited(3)")
        self.close("crash")
        # A clean exit ends the session; the record says when.
        self.launch("quits", prompt="EXIT 0", ok=False)
        self.assertTrue(self.wait_for(lambda: "quits" not in self.names()))
        r = self.run_script("peek", "quits", ok=False)
        self.assertIn("exited with status 0", r.stderr)

    def test_relaunch_reuses_command_and_brief(self):
        self.run_script("launch", "again", "--dir", str(self.repo), "--prompt", "same brief", "--share", "--", *CLAUDE)
        first = self.record("again")["session_id"]
        self.run_script("relaunch", "again")
        rec = self.record("again")
        self.assertNotEqual(rec["session_id"], first)
        self.assertEqual(self.argv("again"), ["--name", "again", "--session-id", rec["session_id"], "--", "same brief"])
        (self.repo / "dirty.txt").write_text("x")
        try:
            self.assertIn("uncommitted changes", self.run_script("relaunch", "again", ok=False).stderr)
        finally:
            (self.repo / "dirty.txt").unlink()
        self.close("again")

    def test_show_field(self):
        self.launch("coord", command=["claude", "--append-system-prompt", "be the coordinator", "--", "{prompt}"],
                    prompt="/coordinator")  # fmt: skip
        self.assertEqual(self.argv("coord"), ["--append-system-prompt", "be the coordinator", "--", "/coordinator"])
        self.assertEqual(self.run_script("show", "coord", "--field", "prompt").stdout, "/coordinator\n")
        self.assertEqual(self.run_script("show", "coord", "--field", "session_id").stdout, "\n")
        self.close("coord")

    @unittest.skipUnless(Path("/proc/self/environ").exists(), "needs /proc")
    def test_close_reports_strays(self):
        self.launch("spawner", prompt="SPAWN a server")
        spawned = self.stub_log("spawner", ".spawned")
        self.assertTrue(self.wait_for(spawned.exists))
        pid = spawned.read_text()
        r = self.run_script("close", "spawner", "--grace", "0.5")
        self.assertIn("still running from the closed session", r.stdout)
        self.assertIn(f"pid {pid}", r.stdout)
        self.assertIn(f"pid {pid}", self.run_script("strays").stdout)
        os.kill(int(pid), signal.SIGKILL)


if __name__ == "__main__":
    if SCRIPT is None:
        sys.exit(__doc__)
    unittest.main(verbosity=2)
