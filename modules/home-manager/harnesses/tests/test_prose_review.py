"""Tests for the prose-review script: the review spec, the outline and the server.

Usage: python3 test_prose_review.py <path/to/prose_review.py>
"""

import importlib.util
import json
import subprocess
import sys
import tempfile
import time
import unittest
import urllib.error
import urllib.request
from pathlib import Path

SCRIPT = Path(sys.argv.pop(1)).resolve() if len(sys.argv) > 1 else None


def load_module():
    spec = importlib.util.spec_from_file_location("prose_review", SCRIPT)
    mod = importlib.util.module_from_spec(spec)
    sys.modules["prose_review"] = mod
    spec.loader.exec_module(mod)
    return mod


DOC = """# Guide

Intro paragraph with *emphasis*.

## Install

Run `make`.

```sh
# not a heading
make install
```

- one
- two

## Use

Final words.
"""


def run(*args, check=True, timeout=30):
    r = subprocess.run(
        [sys.executable, str(SCRIPT), *map(str, args)], capture_output=True, text=True, timeout=timeout
    )
    if check and r.returncode:
        log = Path(str(args[1])) / "server.log" if len(args) > 1 else None
        extra = log.read_text() if log and log.exists() else ""
        raise AssertionError(f"{args}: {r.stdout}{r.stderr}{extra}")
    return r


class ReviewTest(unittest.TestCase):
    def setUp(self):
        self.tmp = tempfile.TemporaryDirectory()
        self.dir = Path(self.tmp.name)
        (self.dir / "doc.md").write_text(DOC)

    def tearDown(self):
        self.tmp.cleanup()

    def spec(self, **extra):
        (self.dir / "review.json").write_text(json.dumps({"file": "doc.md", **extra}))

    def test_outline_skips_fenced_code(self):
        mod = load_module()
        self.assertEqual(mod.outline(DOC), [(1, 1, "Guide"), (5, 2, "Install"), (17, 2, "Use")])
        self.assertEqual(mod.outline("~~~\n# no\n~~~\n## yes ##\n"), [(4, 2, "yes")])

    def test_load_review(self):
        mod = load_module()
        self.spec()
        review = mod.load_review(self.dir)
        self.assertEqual((review.title, review.file, len(review.lines)), ("Guide", self.dir / "doc.md", 19))
        self.spec(title="Handbook", subtitle="Draft")
        self.assertEqual(mod.load_review(self.dir).title, "Handbook")
        (self.dir / "doc.md").write_text("no heading\n")
        self.spec()
        self.assertEqual(mod.load_review(self.dir).title, "doc.md")

    def test_check(self):
        self.spec()
        out = run("check", self.dir).stdout
        self.assertIn("doc.md: 19 lines", out)
        self.assertIn("3 headings", out)
        self.assertIn(" 5  ## Install", out)
        for spec, error in (
            ({"file": "missing.md"}, "error: file: "),
            ({"file": "doc.md", "title": 3}, "error: title: expected a non-empty string"),
            ({}, "error: file: expected a non-empty string"),
            ([], "expected a JSON object"),
        ):
            (self.dir / "review.json").write_text(json.dumps(spec))
            r = run("check", self.dir, check=False)
            self.assertEqual(r.returncode, 1, r.stdout)
            self.assertIn(error, r.stdout)
        (self.dir / "review.json").write_text("{")
        self.assertIn("invalid JSON", run("check", self.dir, check=False).stdout)


class ServerTest(unittest.TestCase):
    def setUp(self):
        self.tmp = tempfile.TemporaryDirectory()
        self.dir = Path(self.tmp.name)
        self.doc = self.dir / "doc.md"
        self.doc.write_text(DOC)
        (self.dir / "review.json").write_text(
            json.dumps({"file": str(self.doc), "source_url": "https://example.test/blob/abc/doc.md#L{line}-L{end}"})
        )
        self.url = run("serve", self.dir, "--detach").stdout.strip()
        self.assertRegex(self.url, r"^http://127\.0\.0\.1:\d+/[\w-]+/$")

    def tearDown(self):
        run("stop", self.dir, check=False)
        self.tmp.cleanup()

    def get(self, path):
        with urllib.request.urlopen(self.url + path, timeout=10) as r:
            return json.load(r)

    def post(self, path, body, headers=None):
        req = urllib.request.Request(
            self.url + path,
            data=json.dumps(body).encode(),
            headers={"Content-Type": "application/json", **(headers or {})},
        )
        with urllib.request.urlopen(req, timeout=10) as r:
            return json.load(r)

    def wait_for(self, fn, timeout=10):
        end = time.time() + timeout
        while time.time() < end:
            v = fn()
            if v:
                return v
            time.sleep(0.2)
        self.fail("timed out")

    def test_round_trip(self):
        root = self.url.rsplit("/", 2)[0]
        with self.assertRaises(urllib.error.HTTPError) as cm:
            urllib.request.urlopen(root + "/wrong-token/api/doc", timeout=5)
        self.assertEqual(cm.exception.code, 404)
        with urllib.request.urlopen(self.url, timeout=5) as r:
            self.assertIn(b"prose.js", r.read())

        doc = self.get("api/doc")
        self.assertEqual(doc["title"], "Guide")
        self.assertEqual(doc["text"], DOC)
        self.assertEqual((doc["version"], doc["oldest"]), (1, 1))
        self.assertEqual(doc["path"], str(self.doc))
        self.assertTrue(doc["sourceUrl"].endswith("#L{line}-L{end}"))
        self.assertEqual(doc["status"], {"session": "away"})

        msg = self.post(
            "api/ask",
            {
                "text": "Tighten this?",
                "refs": [
                    {"kind": "passage", "text": "Run make.", "lo": 7, "hi": 7},
                    {"kind": "passage", "text": "one two", "lo": 15, "hi": 14},
                    {"kind": "passage", "text": "the whole thing"},
                    {"kind": "passage", "text": "off the end", "lo": 99, "hi": 120},
                    {"kind": "quote", "source": "text", "text": "ignored"},
                    {"kind": "passage", "text": "   "},
                ],
            },
        )["message"]
        self.assertEqual(
            [r["label"] for r in msg["refs"]],
            ["L7 Run make.", "L14–15 one two", "the whole thing", "off the end"],
        )
        self.assertEqual([r["lo"] for r in msg["refs"]], [7, 14, None, None])
        out = run("listen", self.dir, "--timeout", 5).stdout
        self.assertIn(f"Question {msg['id']} about {self.doc}", out)
        self.assertIn("Selected text (line 7 of ", out)
        self.assertIn("> Run make.\nSource, line 7:\n```markdown\n7 | Run `make`.\n```", out)
        self.assertIn("Selected text (lines 14–15 of ", out)
        self.assertIn("14 | - one\n15 | - two", out)
        self.assertIn("Selected text:\n> the whole thing", out)
        self.assertIn("Tighten this?", out)
        self.assertIn("edit the file in place", out)
        self.wait_for(lambda: self.get("api/threads")["doc"]["messages"][0]["state"] == "claimed")

        answer = self.dir / "answer.md"
        answer.write_text("Done: now *Run `make`*.")
        out = run("listen", self.dir, "--answer", msg["id"], "--file", answer, "--timeout", 1).stdout
        self.assertIn("Posted the answer", out)
        msgs = self.wait_for(
            lambda: len(self.get("api/threads")["doc"]["messages"]) == 2 and self.get("api/threads")["doc"]["messages"]
        )
        self.assertEqual(msgs[1]["text"], "Done: now *Run `make`*.")
        self.assertEqual(msgs[0]["state"], "answered")
        self.assertEqual(list((self.dir / "queue").glob("q-*")), [])

        second = self.post("api/ask", {"text": "Never mind"})["message"]
        self.post("api/withdraw", {"id": second["id"]})
        self.assertEqual(self.get("api/threads")["doc"]["messages"][2]["state"], "withdrawn")
        self.post("api/clear", {})
        self.assertEqual(self.get("api/threads"), {})

        with self.assertRaises(urllib.error.HTTPError) as cm:
            self.post("api/ask", {"text": "x"}, {"Origin": "http://evil.example"})
        self.assertEqual(cm.exception.code, 403)
        with self.assertRaises(urllib.error.HTTPError) as cm:
            self.post("api/ask", {"text": "", "refs": []})
        self.assertEqual(cm.exception.code, 400)

        self.post("api/finish", {})
        self.assertIn("finished", run("listen", self.dir, "--timeout", 5).stdout)

    def test_edits_reload_and_keep_versions(self):
        edited = DOC.replace("# Guide", "# Handbook").replace("Final words.", "Last words.")
        self.doc.write_text(edited)
        self.wait_for(lambda: self.get("api/doc")["version"] == 2)
        doc = self.get("api/doc")
        self.assertEqual((doc["title"], doc["text"], doc["oldest"]), ("Handbook", edited, 1))
        self.assertEqual(self.get("api/version/1")["text"], DOC)
        self.assertEqual(self.get("api/version/2")["text"], edited)
        with self.assertRaises(urllib.error.HTTPError) as cm:
            self.get("api/version/9")
        self.assertEqual(cm.exception.code, 404)

        # A save without changes is not a new version.
        self.doc.write_text(edited)
        time.sleep(2.5)
        self.assertEqual(self.get("api/doc")["version"], 2)

        # An unreadable document keeps the last good version on the page.
        self.doc.unlink()
        time.sleep(2.5)
        self.assertEqual(self.get("api/doc")["text"], edited)
        self.doc.write_text(DOC)
        self.wait_for(lambda: self.get("api/doc")["version"] == 3)
        self.assertEqual(self.get("api/doc")["title"], "Guide")

    def test_restart_keeps_url(self):
        run("stop", self.dir)
        self.assertEqual(run("serve", self.dir, "--detach").stdout.strip(), self.url)
        self.assertEqual(self.get("api/doc")["title"], "Guide")
        for name in ("server.json", ".server-address"):
            self.assertEqual((self.dir / name).stat().st_mode & 0o777, 0o600)

    def test_listen_times_out_and_reports_presence(self):
        out = run("listen", self.dir, "--timeout", 1).stdout
        self.assertIn("No questions in the last 1 s", out)
        self.assertIn("listen", out.splitlines()[-1])


if __name__ == "__main__":
    unittest.main()
