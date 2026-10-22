"""Tests for the teach-me deck engine: diff parsing, coverage, plain excerpts,
checks and the server.

Usage: python3 test_deck.py <path/to/deck.py>
"""

import copy
import importlib.util
import json
import re
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
    spec = importlib.util.spec_from_file_location("deck", SCRIPT)
    mod = importlib.util.module_from_spec(spec)
    sys.modules["deck"] = mod
    spec.loader.exec_module(mod)
    return mod


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


def run(*args, check=True, timeout=30):
    r = subprocess.run(
        [sys.executable, str(SCRIPT), *map(str, args)], capture_output=True, text=True, timeout=timeout
    )
    if check and r.returncode:
        log = Path(str(args[1])) / "server.log" if len(args) > 1 else None
        extra = log.read_text() if log and log.exists() else ""
        raise AssertionError(f"{args}: {r.stdout}{r.stderr}{extra}")
    return r


class ParseTest(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.rd = load_module()
        cls.files = {f.key: f for f in cls.rd.parse_diff(DIFF)}

    def test_files_and_statuses(self):
        self.assertEqual(
            list(self.files),
            ["src/app.py", "new name.txt", "img.png", "run.sh", "notes.md", "gone.txt"],
        )
        f = self.files
        self.assertEqual(f["src/app.py"].status, "modified")
        self.assertEqual((f["new name.txt"].status, f["new name.txt"].old_path), ("renamed", "old name.txt"))
        self.assertTrue(f["img.png"].binary)
        self.assertEqual(f["run.sh"].status, "mode changed")
        self.assertEqual(f["notes.md"].status, "added")
        self.assertEqual(f["gone.txt"].status, "deleted")
        self.assertEqual(f["gone.txt"].path, "gone.txt")

    def test_rows_and_numbers(self):
        h1, h2 = self.files["src/app.py"].hunks
        self.assertEqual(h1.id, "src/app.py#1")
        self.assertEqual([(r.kind, r.old, r.new) for r in h1.rows[:3]], [(" ", 1, 1), ("-", 2, None), ("+", None, 2)])
        self.assertEqual(h1.rows[-1].new, 7)
        self.assertEqual([(r.old, r.new) for r in h2.rows], [(20, 21), (21, None), (None, 22), (22, 23)])
        notes = self.files["notes.md"].hunks[0]
        self.assertTrue(notes.rows[-1].noeol)
        # The blank added line is a change, but coverage does not require it.
        self.assertEqual((notes.changed(), notes.needed()), ({0, 1, 2}, {0, 2}))
        self.assertEqual(self.files["src/app.py"].adds, 4)
        self.assertEqual(self.files["src/app.py"].dels, 3)

    def test_labels_prefix_keys(self):
        files = self.rd.parse_diff(DIFF, "staged")
        self.assertEqual(files[0].key, "staged:src/app.py")
        self.assertEqual(files[0].hunks[0].id, "staged:src/app.py#1")

    def test_ranges_keep_replacements_whole(self):
        h1 = self.files["src/app.py"].hunks[0]
        rows = lambda r: [(x.kind, x.text.strip()) for x in h1.rows[r[0] : r[1] + 1]]
        # new:2 is the added import; the removed import it replaces comes along.
        self.assertEqual(rows(self.rd.resolve_range(h1, "new:2")), [("-", "import sys"), ("+", "import sys, json")])
        self.assertEqual(rows(self.rd.resolve_range(h1, "old:2")), [("-", "import sys"), ("+", "import sys, json")])
        lo, hi = self.rd.resolve_range(h1, "new:5-6")
        self.assertEqual(h1.rows[lo].kind, "-")
        self.assertEqual(self.rd.resolve_range(h1, "rows:1-3"), (0, 2))
        self.assertEqual(self.rd.resolve_range(h1, None), (0, len(h1.rows) - 1))
        for bad in ("new:100-200", "rows:0-2", "lines:1-2"):
            with self.assertRaises(ValueError):
                self.rd.resolve_range(h1, bad)

    def test_combined_diffs_are_refused(self):
        with self.assertRaises(self.rd.DiffError):
            self.rd.parse_diff("diff --cc file.txt\n")


class DeckTest(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.rd = load_module()

    def setUp(self):
        self.tmp = tempfile.TemporaryDirectory()
        self.dir = Path(self.tmp.name)
        (self.dir / "change.diff").write_text(DIFF)

    def tearDown(self):
        self.tmp.cleanup()

    def write(self, spec):
        (self.dir / "deck.json").write_text(json.dumps(spec))

    def test_full_coverage_builds_appendix(self):
        self.write(deck_spec())
        deck = self.rd.load_deck(self.dir)
        self.assertEqual([s["id"] for s in deck.slides], ["why", "what", "appendix-1"])
        self.assertEqual((deck.slides[-1]["title"], deck.slides[-1]["section"]), ("Everything else", "Appendix"))
        (group,) = deck.slides[-1]["blocks"]
        self.assertEqual((group["type"], group["title"], group["md"]), ("group", "Everything else", "The rest of the change."))
        self.assertEqual([b["file"] for b in group["blocks"]][:2], ["src/app.py", "new name.txt"])
        self.assertEqual(deck.coverage["changed"], 10)
        self.assertEqual(deck.coverage["appendix"], 5)
        r = run("check", self.dir)
        self.assertIn("all 10 non-blank changed lines", r.stdout)

    def test_gaps_are_errors_with_pasteable_fixes(self):
        spec = deck_spec(appendix=[])
        spec["slides"][1]["blocks"] = [{"diff": "src/app.py", "range": "new:1-2"}]
        self.write(spec)
        r = run("check", self.dir, check=False)
        self.assertEqual(r.returncode, 1)
        fixes = [json.loads(m) for m in re.findall(r"add (\{.*\})$", r.stdout, re.M)]
        self.assertIn({"diff": "src/app.py#1", "range": "rows:6-8"}, fixes)
        self.assertIn({"diff": "img.png"}, fixes)
        self.assertIn({"diff": "gone.txt#1"}, fixes)
        spec["appendix"] = [{"title": "Rest", "note": "Everything else.", "diffs": fixes}]
        self.write(spec)
        run("check", self.dir)

    def test_validation_errors(self):
        spec = deck_spec()
        spec["slides"] += [
            {"id": "why", "title": "Again", "blocks": [{"md": "x"}]},
            {"id": "appendix-9", "title": "Reserved", "blocks": [{"md": "x"}]},
            {"id": "9bad", "title": "", "blocks": []},
            {"id": "ok", "title": "Unknown", "blocks": [{"diff": "nope.py"}, {"md": "a", "callout": "b"}]},
            {"id": "ok2", "title": "Diagram", "blocks": [{"diagram": {"type": "steps", "frames": [{"svg": "<svg/>"}]}}]},
        ]
        self.write(spec)
        with self.assertRaises(self.rd.DeckInvalid) as cm:
            self.rd.load_deck(self.dir)
        text = "\n".join(cm.exception.errors)
        for needle in ("duplicate id", "reserved", "starting with a letter", "non-empty string",
                       "non-empty list", "not a file or hunk", "exactly one of", "at least two frames"):
            self.assertIn(needle, text)

    def test_check_slides(self):
        self.write(deck_spec())
        self.assertIn("no check slides", run("check", self.dir).stdout)
        spec = deck_spec()
        spec["slides"].append(CHECK)
        self.write(spec)
        deck = self.rd.load_deck(self.dir)
        self.assertEqual([s["id"] for s in deck.checks()], ["check"])
        q = deck.checks()[0]["quiz"][0]
        self.assertEqual([c["correct"] for c in q["choices"]], [False, True])
        self.assertEqual((q["see"], q["choices"][0]["see"], q["choices"][1]["see"]), ("what", "what", ""))
        self.assertEqual(deck.checks()[0]["blocks"], [])
        self.assertIn("ending with a final check", run("check", self.dir).stdout)
        spec["slides"].insert(1, spec["slides"].pop())
        self.write(spec)
        self.assertIn("last authored slide is not a check", run("check", self.dir).stdout)

    def test_check_validation(self):
        spec = deck_spec()
        q = CHECK["quiz"][0]
        spec["slides"] += [
            {"id": "c1", "title": "Both", "blocks": [{"md": "x"}], "quiz": [q]},
            {"id": "c2", "title": "No key", "quiz": [{**q, "choices": [dict(c, correct=False) for c in q["choices"]]}]},
            {"id": "c3", "title": "Two keys", "quiz": [{**q, "choices": [dict(c, correct=True) for c in q["choices"]]}]},
            {"id": "c4", "title": "Self", "quiz": [{**q, "see": "c4"}]},
            {"id": "c5", "title": "Other check", "quiz": [{**q, "see": "check"}, {**q, "see": "nope"}]},
            {"id": "c6", "title": "One choice", "quiz": [{**q, "choices": q["choices"][:1]}]},
            {"id": "c7", "title": "No why", "quiz": [{**q, "choices": [{"text": "a", "correct": True}, {"text": "b"}]}]},
            {"id": "c8", "title": "Empty", "quiz": []},
            CHECK,
        ]
        self.write(spec)
        with self.assertRaises(self.rd.DeckInvalid) as cm:
            self.rd.load_deck(self.dir)
        errors = cm.exception.errors
        text = "\n".join(errors)
        self.assertIn("slide 'c1': a check slide has quiz, not blocks", text)
        self.assertIn("slide 'c2'.quiz[0].choices: expected exactly one choice with \"correct\": true, found 0", text)
        self.assertIn("slide 'c3'.quiz[0].choices: expected exactly one choice with \"correct\": true, found 2", text)
        self.assertIn("slide 'c4'.quiz[0].see: expected the id of a content slide", text)
        self.assertIn("slide 'c5'.quiz[0].see: expected the id of a content slide", text)
        self.assertIn("slide 'c5'.quiz[1].see: expected the id of a content slide", text)
        self.assertIn("slide 'c6'.quiz[0].choices: expected 2 to 5 choices", text)
        self.assertIn("slide 'c7'.quiz[0].choices[0].why: expected a non-empty string", text)
        self.assertIn("slide 'c8'.quiz: expected a list of 1 to 6 questions", text)
        self.assertFalse([e for e in errors if "'check'" in e], errors)

    def test_labeled_diffs(self):
        (self.dir / "staged.diff").write_text(DIFF)
        spec = deck_spec(diffs=[{"file": "staged.diff", "label": "staged"}])
        spec["slides"][1]["blocks"] = [{"diff": "staged:src/app.py#1"}]
        spec["appendix"][0]["diffs"] = [
            {"diff": "staged:" + d["diff"]} for d in FULL_APPENDIX[0]["diffs"]
        ]
        self.write(spec)
        self.assertEqual(len(self.rd.load_deck(self.dir).slides), 3)


class TopicDeckTest(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.rd = load_module()

    def setUp(self):
        self.tmp = tempfile.TemporaryDirectory()
        self.dir = Path(self.tmp.name)
        (self.dir / "notes.txt").write_text(NOTES)

    def tearDown(self):
        self.tmp.cleanup()

    def write(self, spec):
        (self.dir / "deck.json").write_text(json.dumps(spec))

    def test_plain_excerpts_without_coverage(self):
        repo = self.dir / "repo"
        (repo / "src").mkdir(parents=True)
        (repo / "src" / "lib.py").write_text("a = 1\nb = 2\n")
        # With root set, relative paths resolve there and deck files need absolute paths.
        spec = topic_spec(root="repo")
        spec["slides"][1]["blocks"] = [
            {"md": "Two sources."},
            {"excerpt": str(self.dir / "notes.txt"), "range": "1"},
            {"excerpt": "src/lib.py"},
        ]
        self.write(spec)
        deck = self.rd.load_deck(self.dir)
        self.assertIsNone(deck.coverage)
        ex = deck.slides[1]["blocks"]
        self.assertEqual([b["pieces"][0]["hunk"] for b in ex[1:]], [f"{self.dir / 'notes.txt'}#L1-1", "src/lib.py#L1-2"])
        self.assertIn(repo / "src" / "lib.py", deck.sources)
        payload = self.rd.deck_payload(deck, 1)
        self.assertIsNone(payload["coverage"])
        self.assertEqual(payload["stats"]["files"], 0)
        lib = next(f for f in payload["files"] if f["path"] == "src/lib.py")
        self.assertEqual((lib["plain"], lib["lang"], lib["hunks"][0]["rows"][1]), (True, "python", [" ", None, 2, "b = 2"]))
        out = run("check", self.dir).stdout
        self.assertIn("2 slides and 0 appendix slides\n", out)
        self.assertIn("goal: Explain what the notes say.", out)

    def test_relative_excerpts_resolve_in_the_deck(self):
        self.write(topic_spec())
        h = self.rd.load_deck(self.dir).hunks["notes.txt#L2-3"]
        self.assertEqual([r.text for r in h.rows], ["second line", "third line"])
        self.assertEqual(self.rd.span(h, 0, 1), "2–3")
        self.assertEqual(self.rd.numbered(h, 1, 1), "    3  third line")

    def test_excerpt_errors(self):
        spec = topic_spec()
        spec["slides"][1]["blocks"] = [
            {"excerpt": "notes.txt", "range": "3-9"},
            {"excerpt": "notes.txt", "range": "new:1-2"},
            {"excerpt": "missing.txt"},
        ]
        self.write(spec)
        with self.assertRaises(self.rd.DeckInvalid) as cm:
            self.rd.load_deck(self.dir)
        text = "\n".join(cm.exception.errors)
        self.assertEqual(text.count("expected A-B within lines 1-4 of notes.txt"), 2)
        self.assertIn("cannot read", text)

    def test_goal_required_without_diffs(self):
        spec = topic_spec()
        del spec["goal"]
        self.write(spec)
        r = run("check", self.dir, check=False)
        self.assertEqual(r.returncode, 1)
        self.assertIn("error: goal: expected a non-empty string", r.stdout)
        # A review deck may leave it out; its change bounds it.
        (self.dir / "change.diff").write_text(DIFF)
        self.write(deck_spec())
        run("check", self.dir)

    def test_declared_empty_diffs_skip_coverage(self):
        (self.dir / "change.diff").write_text(DIFF)
        self.write(topic_spec(diffs=[]))
        self.assertIsNone(self.rd.load_deck(self.dir).coverage)
        self.write(topic_spec())
        with self.assertRaises(self.rd.DeckInvalid) as cm:
            self.rd.load_deck(self.dir)
        self.assertTrue(any("is not shown" in e for e in cm.exception.errors))

    def test_final_check_required_past_trivial(self):
        spec = topic_spec()
        spec["slides"] += [{"id": f"more{i}", "title": f"More {i}", "blocks": [{"md": "x"}]} for i in range(2)]
        self.write(spec)
        r = run("check", self.dir, check=False)
        self.assertEqual(r.returncode, 1)
        self.assertIn("must be a final check", r.stdout)
        spec["slides"].append(topic_check())
        spec["slides"].append({"id": "extra", "title": "Extra", "section": "Appendix", "blocks": [{"md": "y"}]})
        self.write(spec)
        out = run("check", self.dir).stdout
        self.assertIn("ending with a final check", out)
        self.assertIn("2. lines: Lines  [checked by final]", out)
        self.assertIn("6. extra: Extra  [appendix]", out)
        self.assertIn("1. intro: Intro  [NOT CHECKED]", out)
        self.assertIn("warning: 3 content slide(s) are not the `see` of any check question", out)
        self.assertIn("intro, more0, more1", out)

    def test_checks_on_every_slide_silence_the_warning(self):
        spec = topic_spec()
        chk = topic_check()
        chk["quiz"][0]["choices"][1]["see"] = "intro"
        spec["slides"].append(chk)
        self.write(spec)
        self.assertEqual([s["id"] for s in self.rd.load_deck(self.dir).unchecked()], [])
        self.assertNotIn("warning", run("check", self.dir).stdout)

    def test_unbalanced_math_warns(self):
        spec = topic_spec()
        chk = topic_check()
        chk["quiz"][0]["choices"][1]["see"] = "intro"
        chk["quiz"][0]["choices"][1]["why"] = "Line 1, so $x_1 is wrong."
        spec["slides"].append(chk)
        spec["slides"][0]["blocks"] = [
            {"md": "Fine: $x_1$, \\(a_{ij}\\), $$a \\\\ b$$, $5 and $6, `$HOME`, \\$x and\n\n```math\nx\n```"},
            {"callout": "Open \\[ x and $$y"},
            {"md": "Unclosed $\\alpha here"},
        ]
        spec["slides"][0]["prompts"] = ["Why \\(y?"]
        self.write(spec)
        out = run("check", self.dir).stdout
        self.assertIn("warning: math delimiters without a match", out)
        self.assertIn("slide 'intro'.blocks[1]: unbalanced math delimiter $$, \\[", out)
        self.assertIn("slide 'intro'.blocks[2]: unbalanced math delimiter $", out)
        self.assertIn("slide 'intro'.prompts[0]: unbalanced math delimiter \\(", out)
        self.assertIn("slide 'final'.quiz[0].choices[1].why: unbalanced math delimiter $", out)
        self.assertNotIn("blocks[0]", out)


class ServerTest(unittest.TestCase):
    def setUp(self):
        self.tmp = tempfile.TemporaryDirectory()
        self.dir = Path(self.tmp.name)
        (self.dir / "change.diff").write_text(DIFF)
        (self.dir / "deck.json").write_text(json.dumps(deck_spec()))
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
            urllib.request.urlopen(root + "/wrong-token/api/deck", timeout=5)
        self.assertEqual(cm.exception.code, 404)
        with urllib.request.urlopen(self.url, timeout=5) as r:
            self.assertIn(b"deck.js", r.read())

        deck = self.get("api/deck")
        self.assertEqual(deck["title"], "Sample change")
        self.assertEqual(deck["status"], {"session": "away"})

        msg = self.post(
            "api/ask",
            {
                "slide": "what",
                "text": "Why json?",
                "refs": [
                    {"kind": "lines", "hunk": "src/app.py#1", "lo": 1, "hi": 2},
                    {"kind": "quote", "source": "callout", "text": "**Risky:** the `json` import"},
                    {"kind": "quote", "source": "elsewhere", "text": "a box"},
                    {"kind": "quote", "source": "math", "text": "$x_1 + a_{ij}$"},
                    {"kind": "lines", "hunk": "nope#1", "lo": 0, "hi": 0},
                ],
            },
        )["message"]
        self.assertEqual([r.get("label") for r in msg["refs"]], ["app.py new 2", None, None, None])
        self.assertEqual(msg["refs"][2]["source"], "diagram")
        out = run("listen", self.dir, "--timeout", 5).stdout
        self.assertIn(f"Question {msg['id']} on slide what", out)
        self.assertIn("+import sys, json", out)
        self.assertIn("Quoted from a callout:\n> **Risky:** the `json` import", out)
        self.assertIn("Quoted from a diagram:\n> a box", out)
        self.assertIn("Quoted from a formula:\n> $x_1 + a_{ij}$", out)
        self.assertIn("Why json?", out)
        self.wait_for(lambda: self.get("api/threads")["what"]["messages"][0]["state"] == "claimed")

        answer = self.dir / "answer.md"
        answer.write_text("To serialize the output.")
        out = run("listen", self.dir, "--answer", msg["id"], "--file", answer, "--timeout", 1).stdout
        self.assertIn("Posted the answer", out)
        msgs = self.wait_for(lambda: len(self.get("api/threads")["what"]["messages"]) == 2 and self.get("api/threads")["what"]["messages"])
        self.assertEqual(msgs[1]["text"], "To serialize the output.")
        self.assertEqual(msgs[0]["state"], "answered")
        self.assertEqual(list((self.dir / "queue").glob("q-*")), [])

        second = self.post("api/ask", {"slide": "why", "text": "Never mind"})["message"]
        self.post("api/withdraw", {"id": second["id"]})
        self.assertEqual(self.get("api/threads")["why"]["messages"][0]["state"], "withdrawn")
        self.post("api/clear", {"slide": "why"})
        self.assertNotIn("why", self.get("api/threads"))

        with self.assertRaises(urllib.error.HTTPError) as cm:
            self.post("api/ask", {"slide": "why", "text": "x"}, {"Origin": "http://evil.example"})
        self.assertEqual(cm.exception.code, 403)

        self.post("api/finish", {})
        self.assertIn("finished", run("listen", self.dir, "--timeout", 5).stdout)

    def test_check_answers(self):
        spec = deck_spec()
        spec["slides"].append(CHECK)
        (self.dir / "deck.json").write_text(json.dumps(spec))
        self.wait_for(lambda: "check" in [s["id"] for s in self.get("api/deck")["slides"]])
        self.assertEqual(self.get("api/quiz"), {})
        for bad in ({"slide": "why", "question": 0, "choice": 0}, {"slide": "check", "question": 1, "choice": 0},
                    {"slide": "check", "question": 0, "choice": 2}, {"slide": "check", "question": "0", "choice": 0}):
            with self.assertRaises(urllib.error.HTTPError) as cm:
                self.post("api/answer", bad)
            self.assertEqual(cm.exception.code, 400)
        self.post("api/answer", {"slide": "check", "question": 0, "choice": 0})
        self.assertEqual(self.get("api/quiz"), {"check": {"0": 0}})
        self.assertEqual(json.loads((self.dir / "quiz.json").read_text())["answers"], {"check": {"0": 0}})
        self.post("api/finish", {})
        out = run("listen", self.dir, "--timeout", 5).stdout
        self.assertIn("finished", out)
        self.assertIn("Checks: 0 of 1 answered correctly.", out)
        self.assertIn("Check: the return value, question 1: What does main return now?", out)
        self.assertIn("chose: Nothing", out)
        self.assertIn("correct: 0", out)
        self.assertIn("covered on slide 2 of 4: What", out)
        self.post("api/answer", {"slide": "check", "question": 0, "choice": None})
        self.assertEqual(self.get("api/quiz"), {"check": {}})
        self.post("api/answer", {"slide": "check", "question": 0, "choice": 1})
        self.post("api/finish", {})
        out = run("listen", self.dir, "--timeout", 5).stdout
        self.assertIn("Checks: 1 of 1 answered correctly.", out)
        self.assertNotIn("chose:", out)

    def test_deck_edits_reload(self):
        spec = deck_spec(title="Renamed")
        (self.dir / "deck.json").write_text(json.dumps(spec))
        self.wait_for(lambda: self.get("api/deck")["title"] == "Renamed")
        version = self.get("api/deck")["version"]
        (self.dir / "deck.json").write_text(json.dumps(deck_spec(appendix=[])))
        time.sleep(2.5)
        deck = self.get("api/deck")
        self.assertEqual((deck["title"], deck["version"]), ("Renamed", version))

    def test_restart_keeps_url(self):
        self.assertEqual(run("serve", self.dir, "--detach").stdout, f"already serving:\n{self.url}\n")
        run("stop", self.dir)
        self.assertIn("received SIGTERM; shutting down", (self.dir / "server.log").read_text())
        self.assertEqual(run("serve", self.dir, "--detach").stdout.strip(), self.url)
        self.assertEqual(self.get("api/deck")["title"], "Sample change")
        for name in ("server.json", ".server-address"):
            self.assertEqual((self.dir / name).stat().st_mode & 0o777, 0o600)

    def test_other_engine_restarts_in_place(self):
        info = json.loads((self.dir / "server.json").read_text())
        (self.dir / "server.json").write_text(json.dumps({**info, "engine": "old", "script": "/old/deck.py"}))
        out = run("listen", self.dir, "--timeout", 1).stdout
        self.assertIn("The server is out of date: it runs /old/deck.py.", out)
        out = run("serve", self.dir, "--detach").stdout
        self.assertEqual(out, f"restarted the server: it runs /old/deck.py\n{self.url}\n")
        restarted = json.loads((self.dir / "server.json").read_text())
        self.assertNotEqual(restarted["pid"], info["pid"])
        self.assertEqual(restarted["engine"], self.get("api/deck")["engine"])
        self.assertNotIn("out of date", run("listen", self.dir, "--timeout", 1).stdout)

    def test_tunnel_hint_persists(self):
        out = run("serve", self.dir, "--detach", "--tunnel", "me@box").stdout
        port = self.url.split(":")[2].split("/")[0]
        hint = f"From another machine, forward the port first: ssh -N -L {port}:127.0.0.1:{port} me@box"
        self.assertEqual(out, f"restarted the server: new options\n{self.url}\n{hint}\n")
        run("stop", self.dir)
        self.assertEqual(run("serve", self.dir, "--detach").stdout, f"{self.url}\n{hint}\n")
        self.assertEqual(run("serve", self.dir, "--detach", "--tunnel", "off").stdout.splitlines()[1:], [self.url])
        self.assertIn("leave out --host", run("serve", self.dir, "--tunnel", "--host", "0.0.0.0", check=False).stderr)

    def test_page_issues_reach_listen_once_per_version(self):
        deck = self.get("api/deck")
        self.assertFalse(deck["audited"])
        issue = {"slide": "what", "where": "diagram 1", "message": "Mermaid: Parse error on line 3"}
        self.post("api/issues", {"version": deck["version"] + 1, "audit": True, "issues": [issue]})
        self.assertFalse(self.get("api/deck")["audited"])
        # One slide's widget error is not a full audit.
        self.post("api/issues", {"version": deck["version"], "issues": []})
        self.assertFalse(self.get("api/deck")["audited"])
        for _ in range(2):
            self.post("api/issues", {"version": deck["version"], "audit": True, "issues": [issue, {**issue, "slide": "nope"}]})
        self.assertTrue(self.get("api/deck")["audited"])
        out = run("listen", self.dir, "--timeout", 5).stdout
        self.assertIn("The page found problems drawing the deck.", out)
        self.assertIn("  slide 2 of 3 (what), diagram 1: Mermaid: Parse error on line 3\n", out)
        self.assertNotIn("nope", out)
        self.assertIn("No questions", run("listen", self.dir, "--timeout", 1).stdout)

    def test_listen_times_out_and_reports_presence(self):
        out = run("listen", self.dir, "--timeout", 1).stdout
        self.assertIn("No questions in the last 1 s", out)
        self.assertIn("listen", out.splitlines()[-1])


class WrittenCheckTest(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.rd = load_module()

    def setUp(self):
        self.tmp = tempfile.TemporaryDirectory()
        self.dir = Path(self.tmp.name)
        (self.dir / "notes.txt").write_text(NOTES)
        (self.dir / "tests").mkdir()
        (self.dir / "tests" / "check.py").write_text(CHECK_PY)

    def tearDown(self):
        self.tmp.cleanup()

    def write(self, quiz):
        spec = topic_spec()
        spec["slides"].append({"id": "final", "title": "Check", "quiz": quiz})
        (self.dir / "deck.json").write_text(json.dumps(spec))

    def test_valid_written_questions(self):
        self.write(written_check()["quiz"])
        prose, code = self.rd.load_deck(self.dir).slide("final")["quiz"]
        self.assertEqual((prose["kind"], prose["choices"], prose["model"]), ("prose", [], "Because it comes after $L_1$."))
        self.assertEqual((code["language"], code["test"]["file"], code["test"]["timeout"]), ("python", "answer.py", 5))

    def test_invalid_written_questions(self):
        quiz = written_check()["quiz"]
        del quiz[0]["rubric"]
        quiz[0]["choices"] = []
        quiz[1]["test"].update(dir="missing", file="../x.py", timeout=0)
        quiz.append({"q": "?", "see": "lines", "kind": "essay"})
        self.write(quiz)
        with self.assertRaises(self.rd.DeckInvalid) as cm:
            self.rd.load_deck(self.dir)
        errors = "\n".join(cm.exception.errors)
        for expected in ("quiz[0]: a prose question has a rubric, not choices", "quiz[0].rubric: expected",
                         "quiz[1].test.dir:", "quiz[1].test.file: expected a file name",
                         "quiz[1].test.timeout:", "quiz[2].kind: expected choice, prose or code"):
            self.assertIn(expected, errors)


class TopicServerTest(unittest.TestCase):
    def setUp(self):
        self.tmp = tempfile.TemporaryDirectory()
        self.dir = Path(self.tmp.name)
        (self.dir / "notes.txt").write_text(NOTES)
        spec = topic_spec()
        spec["slides"].append(topic_check())
        (self.dir / "deck.json").write_text(json.dumps(spec))
        self.url = run("serve", self.dir, "--detach").stdout.strip()

    def tearDown(self):
        run("stop", self.dir, check=False)
        self.tmp.cleanup()

    def test_quoted_plain_lines_reach_listen(self):
        req = urllib.request.Request(
            self.url + "api/ask",
            data=json.dumps({"slide": "lines", "text": "Why?", "refs": [{"kind": "lines", "hunk": "notes.txt#L2-3", "lo": 1, "hi": 1}]}).encode(),
            headers={"Content-Type": "application/json"},
        )
        with urllib.request.urlopen(req, timeout=10) as r:
            msg = json.load(r)["message"]
        self.assertEqual(msg["refs"][0]["label"], "notes.txt 3")
        out = run("listen", self.dir, "--timeout", 5).stdout
        self.assertIn("Selected lines, notes.txt 3:\n```text\n    3  third line\n```", out)
        (self.dir / "a.md").write_text("Because.")
        run("listen", self.dir, "--answer", msg["id"], "--file", self.dir / "a.md", "--timeout", 1)
        with urllib.request.urlopen(self.url + "api/deck", timeout=10) as r:
            self.assertIsNone(json.load(r)["coverage"])

    def post(self, path, body):
        req = urllib.request.Request(self.url + path, data=json.dumps(body).encode(),
                                     headers={"Content-Type": "application/json"})
        with urllib.request.urlopen(req, timeout=10) as r:
            return json.load(r)

    def quiz(self):
        with urllib.request.urlopen(self.url + "api/quiz", timeout=10) as r:
            return json.load(r).get("final", {})

    def wait_for(self, fn, timeout=10):
        end = time.time() + timeout
        while time.time() < end:
            if v := fn():
                return v
            time.sleep(0.2)
        self.fail("timed out")

    def test_written_answers_are_judged_by_the_session(self):
        (self.dir / "tests").mkdir()
        (self.dir / "tests" / "check.py").write_text(CHECK_PY)
        spec = topic_spec()
        spec["slides"].append(written_check())
        (self.dir / "deck.json").write_text(json.dumps(spec))
        self.wait_for(lambda: '"prose"' in urllib.request.urlopen(self.url + "api/deck", timeout=10).read().decode())

        # A resubmission before the session claims the answer replaces it.
        self.post("api/answer", {"slide": "final", "question": 0, "text": "draft"})
        first = self.quiz()["0"]["id"]
        self.post("api/answer", {"slide": "final", "question": 0, "text": "It follows $L_1$."})
        self.assertFalse((self.dir / "queue" / f"{first}.json").exists())
        prose = self.quiz()["0"]
        self.assertEqual((prose["state"], prose["text"]), ("queued", "It follows $L_1$."))

        out = run("listen", self.dir, "--timeout", 5).stdout
        self.assertIn(f"Answer {prose['id']} to check question 1 on slide final", out)
        self.assertIn("Rubric:\nSays it follows line 1.", out)
        self.assertIn("The reader's answer:\n```\nIt follows $L_1$.\n```", out)
        self.assertIn("--verdict correct|partial|incorrect", out)
        self.wait_for(lambda: self.quiz()["0"]["state"] == "judging")
        with self.assertRaises(urllib.error.HTTPError) as cm:
            self.post("api/answer", {"slide": "final", "question": 0, "text": "again"})
        self.assertEqual(cm.exception.code, 409)

        fb = self.dir / "fb.md"
        fb.write_text("Right.")
        r = run("listen", self.dir, "--answer", prose["id"], "--file", fb, "--timeout", 1, check=False)
        self.assertIn("pass --verdict", r.stderr)
        run("listen", self.dir, "--answer", prose["id"], "--verdict", "correct", "--file", fb, "--timeout", 1)
        judged = self.wait_for(lambda: self.quiz()["0"].get("state") == "judged" and self.quiz()["0"])
        self.assertEqual((judged["verdict"], judged["feedback"]), ("correct", "Right."))

        self.post("api/answer", {"slide": "final", "question": 1, "text": "def second():\n    return 'first line'\n"})
        out = run("listen", self.dir, "--timeout", 5).stdout
        self.assertIn("The reader's answer:\n```python\ndef second():", out)
        self.assertIn("Tests (python3 check.py, in ", out)
        self.assertIn("failed with exit status 1", out)
        self.assertIn("AssertionError: first line", out)
        code = self.quiz()["1"]
        self.assertTrue((self.dir / "runs" / code["id"] / "answer.py").exists())
        # A session that lost track of the answer gets it again without rerunning the tests.
        self.assertIn("failed with exit status 1", run("listen", self.dir, "--timeout", 5).stdout)
        fb.write_text("It returns line 1.")
        run("listen", self.dir, "--answer", code["id"], "--verdict", "incorrect", "--file", fb, "--timeout", 1)
        self.wait_for(lambda: self.quiz()["1"].get("verdict") == "incorrect")

        self.post("api/finish", {})
        out = run("listen", self.dir, "--timeout", 5).stdout
        self.assertIn("Checks: 1 of 2 answered correctly.", out)
        self.assertIn("answered (not yet): def second(): return 'first line'", out)

        self.post("api/answer", {"slide": "final", "question": 1, "text": "def second():\n    return 'second line'\n"})
        self.assertIn("Tests (python3 check.py", run("listen", self.dir, "--timeout", 5).stdout)
        self.assertIn(" passed:\n```\nok\n```", (self.dir / "runs" / f"{self.quiz()['1']['id']}.txt").read_text())


if __name__ == "__main__":
    unittest.main()
