"""Fixtures and helpers shared by the tests of walkthrough's deck.py and
walkthrough-pr's walkthrough_pr.py."""

import copy
import importlib.util
import json
import subprocess
import sys
import tempfile
import time
import types
import unittest
import urllib.request
from pathlib import Path


class Script:
    """The script under test, run as a command or loaded as a module."""

    def __init__(self, path: str):
        self.path = Path(path).resolve()
        self._module = None

    def run(self, *args, check=True, timeout=30):
        r = subprocess.run([sys.executable, str(self.path), *map(str, args)], capture_output=True, text=True, timeout=timeout)
        if check and r.returncode:
            log = Path(str(args[1])) / "server.log" if len(args) > 1 else None
            extra = log.read_text() if log and log.exists() else ""
            raise AssertionError(f"{args}: {r.stdout}{r.stderr}{extra}")
        return r

    def module(self):
        """The module, loaded once; an extension's script also exposes the core's names, as `core` does."""
        if self._module:
            return self._module
        spec = importlib.util.spec_from_file_location(self.path.stem, self.path)
        mod = sys.modules[self.path.stem] = importlib.util.module_from_spec(spec)  # dataclasses look it up there
        spec.loader.exec_module(mod)
        core = getattr(mod, "core", None)
        self._module = types.SimpleNamespace(**{**(vars(core) if core else {}), **vars(mod)})
        return self._module


class DeckDirCase(unittest.TestCase):
    """A test with a deck directory holding notes.txt."""

    def setUp(self):
        self.tmp = tempfile.TemporaryDirectory()
        self.addCleanup(self.tmp.cleanup)
        self.dir = Path(self.tmp.name)
        (self.dir / "notes.txt").write_text(NOTES)

    def write(self, spec):
        (self.dir / "deck.json").write_text(json.dumps(spec))


class ServerCase(DeckDirCase):
    """A test with a server running the deck that spec() describes, through `script`."""

    script: Script

    def spec(self) -> dict:
        raise NotImplementedError

    def setUp(self):
        super().setUp()
        self.write(self.spec())
        # Stop the server even when the rest of setUp fails.
        self.addCleanup(self.script.run, "stop", self.dir, check=False)
        self.url = self.script.run("serve", self.dir, "--detach").stdout.strip()
        self.assertRegex(self.url, r"^http://127\.0\.0\.1:\d+/[\w-]+/$")

    def get(self, path):
        with urllib.request.urlopen(self.url + path, timeout=10) as r:
            return json.load(r)

    def page(self, path=""):
        with urllib.request.urlopen(self.url + path, timeout=10) as r:
            return r.read()

    def post(self, path, body, headers=None):
        req = urllib.request.Request(self.url + path, data=json.dumps(body).encode(),
                                     headers={"Content-Type": "application/json", **(headers or {})})
        with urllib.request.urlopen(req, timeout=10) as r:
            return json.load(r)

    def wait_for(self, fn, timeout=10):
        end = time.time() + timeout
        while time.time() < end:
            if v := fn():
                return v
            time.sleep(0.2)
        self.fail("timed out")


# Built line by line so that context lines keep their leading space.
DIFF = "\n".join(
    [
        "diff --git a/src/app.py b/src/app.py",
        "index 1111111..2222222 100644",
        "--- a/src/app.py",
        "+++ b/src/app.py",
        "@@ -1,6 +1,7 @@",
        " import os",
        "-import sys",
        "+import sys, json",
        " ",
        " def main():",
        '-    print("hi")',
        '+    print("hello")',
        "+    return 0",
        " ",
        "@@ -20,3 +21,3 @@ def other():",
        "     a = 1",
        "-    b = 2",
        "+    b = 3",
        "     c = 4",
        "diff --git a/old name.txt b/new name.txt",
        "similarity index 100%",
        "rename from old name.txt",
        "rename to new name.txt",
        "diff --git a/img.png b/img.png",
        "index 3333333..4444444 100644",
        "Binary files a/img.png and b/img.png differ",
        "diff --git a/run.sh b/run.sh",
        "old mode 100644",
        "new mode 100755",
        "diff --git a/notes.md b/notes.md",
        "new file mode 100644",
        "index 0000000..5555555",
        "--- /dev/null",
        "+++ b/notes.md",
        "@@ -0,0 +1,3 @@",
        "+first",
        "+",
        "+second",
        "\\ No newline at end of file",
        "diff --git a/gone.txt b/gone.txt",
        "deleted file mode 100644",
        "index 6666666..0000000",
        "--- a/gone.txt",
        "+++ /dev/null",
        "@@ -1 +0,0 @@",
        "-bye",
        "",
    ]
)

FULL_APPENDIX = [
    {
        "title": "Everything else",
        "note": "The rest of the change.",
        "diffs": [
            {"diff": "src/app.py#2"},
            {"diff": "new name.txt"},
            {"diff": "img.png"},
            {"diff": "run.sh"},
            {"diff": "notes.md"},
            {"diff": "gone.txt"},
        ],
    }
]


CHECK = {
    "id": "check",
    "title": "Check: the return value",
    "section": "What",
    "quiz": [
        {
            "q": "What does `main` return now?",
            "see": "what",
            "choices": [
                {"text": "Nothing", "why": "It gained `return 0`.", "see": "what"},
                {"text": "`0`", "why": "Right: `return 0` was added.", "correct": True},
            ],
        }
    ],
}


def deck_spec(**extra):
    spec = {
        "title": "Sample change",
        "slides": [
            {"id": "why", "title": "Why", "section": "Why", "blocks": [{"md": "Because."}]},
            {
                "id": "what",
                "title": "What",
                "blocks": [{"diff": "src/app.py", "range": "new:1-8"}],
            },
        ],
        "appendix": copy.deepcopy(FULL_APPENDIX),
    }
    spec.update(extra)
    return spec


NOTES = "first line\nsecond line\nthird line\nfourth line\n"


def topic_spec(**extra):
    spec = {
        "title": "Sample topic",
        "goal": "Explain what the notes say.",
        "slides": [
            {"id": "intro", "title": "Intro", "blocks": [{"md": "Hello."}]},
            {"id": "lines", "title": "Lines", "blocks": [{"excerpt": "notes.txt", "range": "2-3"}]},
        ],
    }
    spec.update(extra)
    return spec


def topic_check(see="lines"):
    return {
        "id": "final",
        "title": "Check",
        "quiz": [
            {
                "q": "What is on line 2?",
                "see": see,
                "choices": [
                    {"text": "second line", "why": "Line 2.", "correct": True},
                    {"text": "first line", "why": "That is line 1."},
                ],
            }
        ],
    }


def written_check():
    return {
        "id": "final",
        "title": "Check",
        "quiz": [
            {"q": "Why is line 2 second?", "see": "lines", "kind": "prose",
             "rubric": "Says it follows line 1.", "model": "Because it comes after $L_1$."},
            {"q": "Write `second()` returning line 2.", "see": "lines", "kind": "code", "language": "python",
             "starter": "def second():\n    ...\n", "rubric": "Returns the string.",
             "test": {"dir": "tests", "file": "answer.py", "command": "python3 check.py", "timeout": 5}},
        ],
    }


CHECK_PY = "from answer import second\nassert second() == 'second line', second()\nprint('ok')\n"
