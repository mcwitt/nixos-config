"""Tests for walkthrough-pr's walkthrough_pr.py: diff parsing, coverage, the
appendix, findings and proposed actions, over walkthrough's deck.py.

Usage: python3 test_walkthrough_pr.py <path/to/walkthrough_pr.py>
"""

import copy
import json
import re
import sys
import time
import unittest
import urllib.error
from pathlib import Path

from walkthrough_testing import DeckDirCase, Script, ServerCase, topic_spec

SCRIPT = Script(sys.argv.pop(1))
run = SCRIPT.run

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


class ParseTest(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.rd = SCRIPT.module()
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
        self.assertEqual((self.rd.changed(notes), self.rd.needed(notes)), ({0, 1, 2}, {0, 2}))
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


class DeckTest(DeckDirCase):
    @classmethod
    def setUpClass(cls):
        cls.rd = SCRIPT.module()

    def setUp(self):
        super().setUp()
        (self.dir / "change.diff").write_text(DIFF)

    def test_hunks_reads_diffs_as_the_deck_does(self):
        (self.dir / "change.diff").unlink()
        self.write(topic_spec())
        self.assertIn("0 files, 0 hunks", run("hunks", self.dir).stdout)
        (self.dir / "empty.diff").write_text("")
        self.assertIn("0 files, 0 hunks", run("hunks", self.dir, "--diff", "empty.diff").stdout)
        self.write(topic_spec(diffs=[3]))
        r = run("hunks", self.dir, check=False)
        self.assertEqual((r.returncode, r.stderr.strip()), (1, "diffs[0]: expected a file name or {file, label}"))

    def test_goal_optional_with_diffs(self):
        self.write(deck_spec())
        run("check", self.dir)

    def test_declared_empty_diffs_skip_coverage(self):
        self.write(topic_spec(diffs=[]))
        self.assertNotIn("coverage", self.rd.load_deck(self.dir).extra)
        self.write(topic_spec())
        with self.assertRaises(self.rd.DeckInvalid) as cm:
            self.rd.load_deck(self.dir)
        self.assertTrue(any("is not shown" in e for e in cm.exception.errors))

    def test_full_coverage_builds_appendix(self):
        self.write(deck_spec())
        deck = self.rd.load_deck(self.dir)
        self.assertEqual([s["id"] for s in deck.slides], ["why", "what", "appendix-1", "findings", "proposed-actions"])
        self.assertEqual([s["blocks"] for s in deck.slides[-2:]], [[{"type": "findings"}], [{"type": "actions"}]])
        self.assertEqual((deck.slides[2]["title"], deck.slides[2]["section"]), ("Everything else", "Appendix"))
        (group,) = deck.slides[2]["blocks"]
        self.assertEqual((group["type"], group["title"], group["md"]), ("group", "Everything else", "The rest of the change."))
        self.assertEqual([b["file"] for b in group["blocks"]][:2], ["src/app.py", "new name.txt"])
        self.assertEqual(deck.extra["coverage"]["changed"], 10)
        self.assertEqual(deck.extra["coverage"]["appendix"], 5)
        r = run("check", self.dir)
        self.assertIn("all 10 non-blank changed lines", r.stdout)

    def test_pr_and_reserved_ids(self):
        spec = deck_spec(pr="https://github.com/o/r/issues/3")
        spec["slides"][0]["id"] = "findings"
        self.write(spec)
        with self.assertRaises(self.rd.DeckInvalid) as cm:
            self.rd.load_deck(self.dir)
        errors = "\n".join(cm.exception.errors)
        self.assertIn("pr: expected a pull request URL", errors)
        self.assertIn("'findings' is reserved for a generated slide", errors)
        self.write(deck_spec(pr="https://github.com/o/r/pull/3"))
        self.assertEqual(self.rd.load_deck(self.dir).spec["pr"], "https://github.com/o/r/pull/3")

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
            {"id": "appendix-9", "title": "Reserved", "blocks": [{"md": "x"}]},
            {"id": "ok", "title": "Unknown", "blocks": [{"diff": "nope.py"}]},
        ]
        self.write(spec)
        with self.assertRaises(self.rd.DeckInvalid) as cm:
            self.rd.load_deck(self.dir)
        text = "\n".join(cm.exception.errors)
        for needle in ("'appendix-9' is reserved", "not a file or hunk"):
            self.assertIn(needle, text)

    def test_check_points_at_diff_slides(self):
        self.write(deck_spec(slides=[*deck_spec()["slides"], CHECK]))
        q = self.rd.load_deck(self.dir).checks()[0]["quiz"][0]
        self.assertEqual((q["see"], q["choices"][0]["see"]), ("what", "what"))
        self.assertIn("ending with a final check", run("check", self.dir).stdout)
        # The generated appendix and review slides after it do not count as authored.
        self.write(deck_spec(slides=[deck_spec()["slides"][0], CHECK, deck_spec()["slides"][1]]))
        self.assertIn("last authored slide is not a check", run("check", self.dir).stdout)

    def test_labeled_diffs(self):
        (self.dir / "staged.diff").write_text(DIFF)
        spec = deck_spec(diffs=[{"file": "staged.diff", "label": "staged"}])
        spec["slides"][1]["blocks"] = [{"diff": "staged:src/app.py#1"}]
        spec["appendix"][0]["diffs"] = [
            {"diff": "staged:" + d["diff"]} for d in FULL_APPENDIX[0]["diffs"]
        ]
        self.write(spec)
        self.assertEqual(len(self.rd.load_deck(self.dir).slides), 5)
        # A place named by path finds the lines of a labeled diff.
        self.assertEqual(self.rd.locate(self.rd.load_deck(self.dir), "src/app.py:2")["hunk"], "staged:src/app.py#1")


class ReviewServerTest(ServerCase):
    script = SCRIPT

    def spec(self):
        (self.dir / "change.diff").write_text(DIFF)
        return deck_spec()

    def test_round_trip(self):
        # The review layer's page loads after the core's.
        self.assertIn(b'<script src="deck.js" defer></script>\n<link rel="stylesheet" href="review.css">\n<script src="review.js" defer></script>', self.page())
        for name in ("review.js", "review.css"):
            self.assertGreater(len(self.page(name)), 1000)
        deck = self.get("api/deck")
        self.assertEqual((deck["coverage"]["changed"], deck["stats"]["files"]), (10, 6))
        msg = self.post("api/ask", {"slide": "what", "text": "Why json?", "refs": [
            {"kind": "lines", "hunk": "src/app.py#1", "lo": 1, "hi": 2}, {"kind": "lines", "hunk": "nope#1", "lo": 0, "hi": 0}]})["message"]
        self.assertEqual([r["label"] for r in msg["refs"]], ["app.py new 2"])
        out = run("listen", self.dir, "--timeout", 5).stdout
        self.assertIn("+import sys, json", out)

    def test_other_script_and_engine(self):
        info = json.loads((self.dir / "server.json").read_text())
        (self.dir / "server.json").write_text(json.dumps({**info, "engine": "old", "script": "/old/walkthrough_pr.py"}))
        r = run("finding", self.dir, "add", "x", check=False)
        self.assertIn("The server is out of date (it runs /old/walkthrough_pr.py)", r.stderr)
        # A restart in place keeps the review layer, and the reloaded deck keeps its coverage.
        self.assertIn("restarted the server", run("serve", self.dir, "--detach").stdout)
        self.assertIn(b"review.js", self.page())
        self.write(deck_spec(title="Renamed"))
        self.wait_for(lambda: self.get("api/deck")["title"] == "Renamed")
        self.assertEqual(self.get("api/deck")["coverage"]["changed"], 10)
        # The core's own script cannot handle the review's requests.
        core = Script(str(Path(SCRIPT.module().CORE) / "deck.py"))
        r = core.run("listen", self.dir, "--timeout", 1, check=False)
        self.assertIn(f"This deck is served by {SCRIPT.path}", r.stderr)
        # A server from before the rename, by its old name, is out of date rather than another script's.
        info = json.loads((self.dir / "server.json").read_text())
        old = {k: v for k, v in info.items() if k != "extensions"}
        (self.dir / "server.json").write_text(json.dumps({**old, "engine": "old", "script": "/old/review_deck.py"}))
        self.assertIn("The server is out of date", run("listen", self.dir, "--timeout", 1).stdout)
        (self.dir / "server.json").write_text(json.dumps(info))  # so that the cleanup's `stop` recognises it

    def test_check_report_counts_generated_slides(self):
        self.write(deck_spec(slides=[*deck_spec()["slides"], CHECK]))
        self.wait_for(lambda: "check" in [s["id"] for s in self.get("api/deck")["slides"]])
        self.post("api/answer", {"slide": "check", "question": 0, "choice": 0})
        self.post("api/finish", {})
        self.assertIn("covered on slide 2 of 6: What", run("listen", self.dir, "--timeout", 5).stdout)

    def test_findings(self):
        added = self.post("api/review", {"op": "add", "text": "json is unused", "slide": "what",
                                           "refs": [{"kind": "lines", "hunk": "src/app.py#1", "lo": 1, "hi": 2}]})["finding"]
        self.assertEqual((added["sources"], added["severity"], added["slide"]), (["reader"], None, "what"))
        ref = added["refs"][0]
        self.assertEqual({k: ref[k] for k in ("path", "side", "line", "end")}, {"path": "src/app.py", "side": "new", "line": 2, "end": 2})
        out = run("listen", self.dir, "--timeout", 5).stdout
        self.assertIn(f"The reader added a finding without a severity:\n  {added['id']} (no severity, from reader, at app.py new 2): json is unused", out)
        self.assertIn(f"finding {self.dir} set {added['id']} --severity P2", out)
        dropped = self.post("api/review", {"op": "add", "text": "never mind"})["finding"]
        self.post("api/review", {"op": "remove", "id": dropped["id"]})
        self.assertIn("No questions", run("listen", self.dir, "--timeout", 1).stdout)
        out = run("finding", self.dir, "set", added["id"], "--severity", "P3").stdout
        self.assertIn(f"Saved {added['id']} (P3", out)

        out = run("finding", self.dir, "add", "b is now 3", "--severity", "P1", "--at", "src/app.py:22", "--at", "src/app.py:90-95").stdout
        mine = json.loads((self.dir / "findings.json").read_text())["findings"][1]
        self.assertIn(f"Saved {mine['id']} (P1, from reader, at app.py new 22, app.py 90–95): b is now 3", out)
        self.assertEqual([r["kind"] for r in mine["refs"]], ["lines", "place"])
        self.assertEqual((mine["refs"][1]["line"], mine["refs"][1]["end"]), (90, 95))
        self.assertIn("No questions", run("listen", self.dir, "--timeout", 1).stdout)

        for bad in ({"op": "set", "id": mine["id"], "severity": "P9"}, {"op": "set", "id": mine["id"], "choices": ["comment"]},
                    {"op": "set", "id": mine["id"], "choices": ["ignore"]}, {"op": "set", "id": "f-nope", "choices": ["fix"]},
                    {"op": "add", "text": " "}, {"op": "frob", "id": mine["id"]}):
            with self.assertRaises(urllib.error.HTTPError) as cm:
                self.post("api/review", bad)
            self.assertEqual(cm.exception.code, 400)
        r = run("finding", self.dir, "set", "f-nope", "--severity", "P1", check=False)
        self.assertEqual((r.returncode, r.stderr.strip()), (1, "no finding f-nope"))
        # Choosing Fix asks the session to draft it straight away.
        self.post("api/review", {"op": "set", "id": mine["id"], "choices": ["fix"]})
        self.assertEqual(self.get("api/review")["proposal"], "requested")
        self.assertIn(f"{mine['id']} (P1, from reader, at app.py new 22, app.py 90–95): b is now 3 [fix]", run("listen", self.dir, "--timeout", 5).stdout)
        self.post("api/review", {"op": "remove", "id": added["id"]})
        self.assertEqual([f["id"] for f in self.get("api/review")["findings"]], [mine["id"]])
        self.assertIn("[fix]", run("finding", self.dir, "list").stdout)

        self.post("api/finish", {})
        out = run("listen", self.dir, "--timeout", 5).stdout
        self.assertIn(f"Findings:\n  {mine['id']} (P1", out)

    def test_review_findings_stay_hidden_until_revealed(self):
        F = lambda *a, **k: run("finding", self.dir, *a, **k)
        F("review", "running")
        mine = self.post("api/review", {"op": "add", "text": "P1 json unused", "severity": "P2",
                                          "refs": [{"kind": "lines", "hunk": "src/app.py#1", "lo": 1, "hi": 2}]})["finding"]
        F("add", "`json` is imported but never used", "--review", "--severity", "P1", "--at", "src/app.py:2")
        F("add", "`b` changed from 2 to 3 without a test", "--review", "--severity", "P3", "--at", "src/app.py:22")
        page = self.get("api/review")
        self.assertEqual(([f["id"] for f in page["findings"]], page["hidden"], page["review"], page["reveal"]),
                         ([mine["id"]], 2, "running", "hidden"))
        review = [f for f in json.loads((self.dir / "findings.json").read_text())["findings"] if f["sources"] == ["review"]]
        for bad in ({"op": "merge", "id": mine["id"], "others": [review[0]["id"]]}, {"op": "reveal"},
                    {"op": "remove", "id": review[0]["id"]}, {"op": ["add"]},
                    {"op": "set", "id": mine["id"], "severity": "P0", "choices": ["comment"]}):
            with self.assertRaises(urllib.error.HTTPError):
                self.post("api/review", bad)

        self.assertEqual(self.get("api/review")["findings"][0]["severity"], "P2")  # the rejected set changed nothing
        self.assertEqual(self.post("api/review", {"op": "request-reveal"}), {"reveal": "requested"})
        out = run("listen", self.dir, "--timeout", 5).stdout
        self.assertIn("The reader asked to see the review's findings.", out)
        self.assertIn("The background review is still running.", out)
        self.assertIn(f"The reader's findings:\n  {mine['id']} (P2, from reader", out)
        self.assertIn(f"  {review[0]['id']} (P1, from review, hidden, at app.py new 2): `json` is imported", out)
        self.assertIn(f"finding {self.dir} merge KEEP_ID OTHER_ID...", out)

        # The request comes again when the review is done, and when the reader asks again, but only once at a time.
        self.assertIn("No questions", run("listen", self.dir, "--timeout", 1).stdout)
        F("review", "done")
        self.post("api/review", {"op": "request-reveal"})
        self.assertIn("asked to see the review's findings", run("listen", self.dir, "--timeout", 5).stdout)
        self.assertIn("No questions", run("listen", self.dir, "--timeout", 1).stdout)
        merged = F("merge", mine["id"], review[0]["id"], review[0]["id"]).stdout
        self.assertIn(f"Saved {mine['id']} (P1, from reader and review, at app.py new 2): P1 json unused", merged)
        self.assertEqual(self.get("api/review")["hidden"], 1)
        self.assertIn("merge needs", F("merge", mine["id"], mine["id"], check=False).stderr)
        self.post("api/finish", {})
        self.assertIn("The reader has not seen the review's findings.", run("listen", self.dir, "--timeout", 5).stdout)
        F("reveal")
        self.assertIn("No questions", run("listen", self.dir, "--timeout", 1).stdout)
        self.post("api/review", {"op": "set", "id": mine["id"], "text": "json is unused"})
        page = self.get("api/review")
        self.assertEqual((len(page["findings"]), page["hidden"], page["reveal"]), (2, 0, "shown"))
        self.assertEqual(page["findings"][0]["merged"][0]["text"], "`json` is imported but never used")
        # Later review findings show at once, and a second request changes nothing.
        F("add", "late", "--review")
        self.assertEqual(self.get("api/review")["hidden"], 0)
        self.assertEqual(self.post("api/review", {"op": "request-reveal"}), {"reveal": "shown"})
        self.assertIn("review: done, findings shown", F("list").stdout)

    def test_findings_from_before_the_review_merge(self):
        run("stop", self.dir)
        old = {"id": "f-1-aa", "text": "old", "severity": "P2", "sources": ["reader"], "refs": [], "slide": "", "at": 0, "action": "comment"}
        ignored = {**old, "id": "f-1-bb", "action": "ignore"}
        # An action sent before the log existed counts as taken, since no queued request holds it.
        sent = {"id": "a-1-cc", "kind": "check", "title": "t", "finding": None, "include": True, "state": "running",
                "result": None, "command": "true", "sent": time.time()}
        (self.dir / "findings.json").write_text(json.dumps({"version": 1, "findings": [old, ignored], "actions": [sent]}))
        run("serve", self.dir, "--detach")
        self.wait_for(lambda: self.get("api/review")["actions"][0].get("started"))
        self.wait_for(lambda: self.get("api/deck")["status"]["session"] == "working")
        run("action", self.dir, "result", "a-1-cc", "--status", "done")
        self.wait_for(lambda: self.get("api/deck")["status"]["session"] == "away")
        run("finding", self.dir, "add", "dup", "--review")
        review = json.loads((self.dir / "findings.json").read_text())["findings"][2]["id"]
        self.assertIn("from reader and review", run("finding", self.dir, "merge", "f-1-aa", review).stdout)
        # Who wrote an old finding's text is unknown, so its comment would post from the agent's account;
        # its old single choice becomes a list.
        self.assertEqual({k: self.get("api/review")["findings"][0].get(k) for k in ("author", "choices", "action")},
                         {"author": "session", "choices": ["comment"], "action": None})
        self.assertEqual(self.get("api/review")["findings"][1]["choices"], [])

    def test_finishing_while_the_review_runs(self):
        run("finding", self.dir, "review", "running")
        self.post("api/finish", {})
        self.assertIn("The background review is still running", run("listen", self.dir, "--timeout", 5).stdout)

    def test_reveal_without_review_findings_is_immediate(self):
        self.assertEqual(self.post("api/review", {"op": "request-reveal"}), {"reveal": "shown"})
        self.assertIn("No questions", run("listen", self.dir, "--timeout", 1).stdout)

    def test_choices_ask_for_drafts(self):
        (self.dir / "deck.json").write_text(json.dumps(deck_spec(pr="https://github.com/o/r/pull/7")))
        self.wait_for(lambda: self.get("api/deck")["pr"])
        A = lambda *a, **k: run("action", self.dir, *a, **k)
        f, g = (self.post("api/review", {"op": "add", "text": t, "severity": "P3"})["finding"] for t in ("one", "two"))
        self.post("api/review", {"op": "set", "id": f["id"], "choices": ["fix"]})
        self.assertIn("[fix]", run("listen", self.dir, "--timeout", 5).stdout)
        # A choice made while the session drafts keeps the drafting going.
        self.post("api/review", {"op": "set", "id": g["id"], "choices": ["fix"]})
        A("ready")
        self.assertEqual(self.get("api/review")["proposal"], "requested")
        run("listen", self.dir, "--timeout", 5)
        A("ready")
        self.assertEqual(self.get("api/review")["proposal"], "ready")
        # The session records a choice the reader asks for in words.
        run("finding", self.dir, "add", "three", "--choose", "comment")
        self.assertEqual(self.get("api/review")["findings"][-1]["choices"], ["comment"])
        self.assertIn("three [comment]", run("listen", self.dir, "--timeout", 5).stdout)
        # Toggling one kind leaves the finding's other drafts alone.
        A("add", "check", "--finding", f["id"], "--title", "t", "--command", "true")
        self.post("api/review", {"op": "set", "id": f["id"], "choices": ["comment", "fix"]})
        self.post("api/review", {"op": "set", "id": f["id"], "choices": []})
        self.assertTrue(self.get("api/review")["actions"][0]["include"])

    def test_proposed_actions(self):
        (self.dir / "deck.json").write_text(json.dumps(deck_spec(pr="https://github.com/o/r/pull/7")))
        self.wait_for(lambda: self.get("api/deck")["pr"])
        A = lambda *a, **k: run("action", self.dir, *a, **k)
        f = self.post("api/review", {"op": "add", "text": "json is unused", "severity": "P2",
                                     "refs": [{"kind": "lines", "hunk": "src/app.py#1", "lo": 1, "hi": 2}]})["finding"]
        self.post("api/review", {"op": "set", "id": f["id"], "choices": ["comment"]})
        self.assertEqual(self.get("api/review")["proposal"], "requested")
        out = run("listen", self.dir, "--timeout", 5).stdout
        self.assertIn("Findings have choices without drafts.", out)
        self.assertIn(f"  {f['id']} (P2, from reader, at app.py new 2): json is unused [comment] (in the reader's words)", out)
        self.assertIn(f"action {self.dir} add comment --finding ID --body-file FILE", out)

        (self.dir / "body.md").write_text("`json` is imported but unused; drop it.")
        out = A("add", "comment", "--finding", f["id"], "--body-file", self.dir / "body.md").stdout
        comment = self.get("api/review")["actions"][0]
        self.assertIn(f"Saved {comment['id']} comment on {f['id']}: Comment at app.py new 2 [proposed]", out)
        self.assertEqual((comment["author"], comment["place"]["path"], comment["place"]["side"], comment["place"]["line"]),
                         ("session", "src/app.py", "new", 2))
        (self.dir / "fix.diff").write_text("--- a/src/app.py\n+++ b/src/app.py\n@@ -1,2 +1,2 @@\n import os\n-import sys, json\n+import sys\n")
        A("add", "fix", "--finding", f["id"], "--title", "Drop the import", "--patch-file", self.dir / "fix.diff")
        (self.dir / "bad.diff").write_text("not a diff\n")
        self.assertIn("patch:", A("add", "fix", "--title", "x", "--patch-file", self.dir / "bad.diff", check=False).stderr)
        A("add", "check", "--title", "Nothing imports json", "--command", "grep -rn json src")
        A("ready")
        page = self.get("api/review")
        comment, fix, check = page["actions"]
        self.assertEqual(page["proposal"], "ready")
        self.assertEqual([b["pieces"][0]["hunk"] for b in fix["diff"]["blocks"]], [f"{fix['id']}:src/app.py#1"])

        # The reader quotes the fix's lines, rewords the comment and skips the check.
        q = self.post("api/ask", {"slide": "proposed-actions", "text": "why?", "refs": [{"kind": "lines", "hunk": fix["diff"]["blocks"][0]["pieces"][0]["hunk"], "lo": 1, "hi": 2}]})["message"]
        self.assertIn("+import sys", run("listen", self.dir, "--timeout", 5).stdout)
        self.assertEqual(q["refs"][0]["path"], "src/app.py")
        (self.dir / "a.md").write_text("Because.")
        run("listen", self.dir, "--answer", q["id"], "--file", self.dir / "a.md", "--timeout", 1)
        # Saving the agent's text unchanged keeps it the agent's; changing it makes it the reader's.
        self.post("api/review", {"op": "action-set", "id": comment["id"], "body": comment["body"]})
        self.assertEqual(self.get("api/review")["actions"][0]["author"], "session")
        self.post("api/review", {"op": "action-set", "id": comment["id"], "body": "Please drop the unused `json` import."})
        # A finding can point into a proposed fix.
        f2 = self.post("api/review", {"op": "add", "text": "keep sys?", "severity": "P3",
                                      "refs": [{"kind": "lines", "hunk": fix["diff"]["blocks"][0]["pieces"][0]["hunk"], "lo": 2, "hi": 2}]})["finding"]
        self.assertEqual(f2["refs"][0]["line"], 2)
        # Its comment cannot sit on lines the pull request does not show, so it goes in the summary.
        self.assertIn("Comment in the review summary", A("add", "comment", "--finding", f2["id"], "--body-file", self.dir / "body.md").stdout)
        A("remove", self.get("api/review")["actions"][-1]["id"])
        # Without a body, a comment takes its finding's text and whoever wrote it.
        A("add", "comment", "--finding", f2["id"])
        self.assertEqual({k: self.get("api/review")["actions"][-1][k] for k in ("body", "author", "include")},
                         {"body": "keep sys?", "author": "reader", "include": False})  # not marked Comment
        self.post("api/review", {"op": "set", "id": f2["id"], "choices": ["comment"]})  # a draft exists, so nothing to ask
        # Rewording the finding rewords the draft that still says the same, as whoever reworded it;
        # a draft with its own text keeps it.
        A("add", "comment", "--finding", f2["id"], "--body-file", self.dir / "body.md")
        last2 = lambda: [{k: a[k] for k in ("body", "author", "include")} for a in self.get("api/review")["actions"][-2:]]
        self.post("api/review", {"op": "set", "id": f2["id"], "text": "keep sys??"})
        self.assertEqual(last2()[0], {"body": "keep sys??", "author": "reader", "include": True})
        run("finding", self.dir, "set", f2["id"], "--text", "Is sys still needed?")
        self.assertEqual(last2(), [{"body": "Is sys still needed?", "author": "session", "include": True},
                                   {"body": (self.dir / "body.md").read_text(), "author": "session", "include": True}])
        # A finding's drafts go out only while it is marked for their kind.
        self.post("api/review", {"op": "set", "id": f2["id"], "choices": []})
        self.assertEqual([a["include"] for a in last2()], [False, False])
        self.post("api/review", {"op": "set", "id": f2["id"], "choices": ["comment"]})
        self.assertEqual([a["include"] for a in last2()], [True, True])
        for a in self.get("api/review")["actions"][-2:]:
            A("remove", a["id"])
        self.post("api/review", {"op": "action-set", "id": check["id"], "include": False})
        with self.assertRaises(urllib.error.HTTPError):
            self.post("api/review", {"op": "action-set", "id": comment["id"], "title": "mine now"})
        self.assertEqual(self.get("api/review")["actions"][0]["author"], "reader")

        self.post("api/review", {"op": "carry-out", "ids": [comment["id"], fix["id"]]})
        actions = self.get("api/review")["actions"]
        self.assertEqual([a["state"] for a in actions], ["running", "running", "proposed"])
        self.assertGreater(actions[0]["resend_in"], 1000)
        self.assertEqual(actions[2]["resend_in"], 0)
        with self.assertRaises(urllib.error.HTTPError) as cm:
            self.post("api/review", {"op": "carry-out", "ids": [comment["id"]]})
        self.assertIn(b"waiting for the session to take it", cm.exception.read())
        sent = self.get("api/review")["log"][-1]
        self.assertEqual((sent["event"], sent["actions"], sent.get("started")), ("sent", [comment["id"], fix["id"]], None))
        out = run("listen", self.dir, "--timeout", 5).stdout
        self.assertIn("The reader approved these actions.", out)
        self.assertIn("posts as the reader's account, at src/app.py:2 (new side):\n    > Please drop the unused `json` import.", out)
        self.assertIn("-import sys, json", out)
        self.assertNotIn("grep -rn json", out)
        self.assertIn("on https://github.com/o/r/pull/7 as review comments", out)
        # The server notices the request was taken on its next tick.
        deadline = time.time() + 5
        while not self.get("api/review")["log"][-1].get("started") and time.time() < deadline:
            time.sleep(0.1)
        self.assertTrue(self.get("api/review")["log"][-1].get("started"))
        self.assertTrue(all(a.get("started") for a in self.get("api/review")["actions"][:2]))
        self.wait_for(lambda: self.get("api/deck")["status"]["session"] == "listening")  # `listen` ran just now
        with self.assertRaises(urllib.error.HTTPError) as cm:
            self.post("api/review", {"op": "carry-out", "ids": [comment["id"]]})
        self.assertIn(b"sent to the session just now", cm.exception.read())
        self.assertIn("is proposed, not running", A("result", check["id"], "--status", "done", check=False).stderr)
        self.assertIn("is proposed, not running", A("progress", check["id"], "--note", "x", check=False).stderr)
        A("progress", fix["id"], "--note", "applying the patch")
        A("result", comment["id"], "--status", "done", "--link", "https://github.com/o/r/pull/7#discussion_r1")
        self.assertEqual([(e["event"], e.get("action"), e.get("note") or e.get("link")) for e in self.get("api/review")["log"][-2:]],
                         [("progress", fix["id"], "applying the patch"), ("done", comment["id"], "https://github.com/o/r/pull/7#discussion_r1")])
        actions = self.get("api/review")["actions"]
        self.assertEqual((actions[0]["state"], actions[0]["result"]), ("done", {"link": "https://github.com/o/r/pull/7#discussion_r1"}))
        self.assertIn("is done", A("set", comment["id"], "--title", "x", check=False).stderr)
        with self.assertRaises(urllib.error.HTTPError):
            self.post("api/review", {"op": "carry-out", "ids": [comment["id"]]})
        self.assertIn("[done]", A("list").stdout)

    def test_actions_follow_merged_findings(self):
        (self.dir / "deck.json").write_text(json.dumps(deck_spec(pr="https://github.com/o/r/pull/7")))
        self.wait_for(lambda: self.get("api/deck")["pr"])
        a = self.post("api/review", {"op": "add", "text": "first", "severity": "P2"})["finding"]
        b = self.post("api/review", {"op": "add", "text": "second", "severity": "P2"})["finding"]
        (self.dir / "body.md").write_text("Body.")
        for f in (a, b):
            self.post("api/review", {"op": "set", "id": f["id"], "choices": ["comment"]})
            run("action", self.dir, "add", "comment", "--finding", f["id"], "--body-file", self.dir / "body.md")
        run("action", self.dir, "add", "check", "--finding", b["id"], "--title", "t", "--command", "true")
        run("finding", self.dir, "merge", a["id"], b["id"])
        # The merged finding keeps one comment draft, and drafts for the folded one still find it.
        actions = self.get("api/review")["actions"]
        self.assertEqual([(x["kind"], x["finding"]) for x in actions], [("comment", a["id"]), ("check", a["id"])])
        self.assertIn(a["id"], run("action", self.dir, "add", "comment", "--finding", b["id"], "--body-file", self.dir / "body.md").stdout)
        run("action", self.dir, "remove", self.get("api/review")["actions"][-1]["id"])
        # Removing it drops its unsent drafts; what went out stays, unlinked.
        self.post("api/review", {"op": "carry-out", "ids": [actions[1]["id"]]})
        run("action", self.dir, "result", actions[1]["id"], "--status", "done")
        self.post("api/review", {"op": "remove", "id": a["id"]})
        self.assertEqual([(x["kind"], x["finding"]) for x in self.get("api/review")["actions"]], [("check", None)])
        self.assertIn("removed, or never existed", run("action", self.dir, "add", "check", "--finding", a["id"], "--title", "t", "--command", "true", check=False).stderr)

    def test_choices_edge_cases(self):
        (self.dir / "deck.json").write_text(json.dumps(deck_spec(pr="https://github.com/o/r/pull/7")))
        self.wait_for(lambda: self.get("api/deck")["pr"])
        F = lambda *a, **k: run("finding", self.dir, *a, **k)
        A = lambda *a, **k: run("action", self.dir, *a, **k)
        api = lambda op: self.post("api/review", op)
        # A hidden finding takes no choices, and a rejected add leaves nothing behind.
        self.assertIn("not seen", F("add", "x", "--review", "--choose", "fix", check=False).stderr)
        self.assertEqual(json.loads((self.dir / "findings.json").read_text() if (self.dir / "findings.json").exists() else '{"findings": []}')["findings"], [])
        f = api({"op": "add", "text": "one", "severity": "P3"})["finding"]
        # One kind turned on or off keeps the others, whatever the page last saw.
        api({"op": "set", "id": f["id"], "choose": "fix", "on": True})
        api({"op": "set", "id": f["id"], "choose": "comment", "on": True})
        self.assertEqual(self.get("api/review")["findings"][0]["choices"], ["comment", "fix"])
        # Turned off before the session picks it up, the request is withdrawn.
        F("set", f["id"], "--choose", "")
        self.assertEqual((self.get("api/review")["proposal"], [p.name for p in (self.dir / "queue").glob("*.json") if p.name != "pages.json"]), ("none", []))
        # A draft turned off while it was drafted arrives left out; Include and the choice move together.
        api({"op": "set", "id": f["id"], "choose": "fix", "on": True})
        self.assertTrue(self.get("api/review")["waiting"])
        # The page hears when the session takes the request, though no edit comes with it.
        with urllib.request.urlopen(self.url + "api/events", timeout=10) as events:
            self.assertEqual(events.readline().strip(), b"event: hello")
            run("listen", self.dir, "--timeout", 5)
            seen = []
            while b"event: review\n" not in seen:
                try:
                    seen.append(events.readline())
                except TimeoutError:
                    self.fail(f"no review event: {seen}")
        # Taken by the session, the request is no longer waiting, though drafting goes on.
        self.assertEqual((self.get("api/review")["waiting"], self.get("api/review")["proposal"]), (False, "requested"))
        api({"op": "set", "id": f["id"], "choose": "fix", "on": False})
        (self.dir / "fix.diff").write_text("--- a/src/app.py\n+++ b/src/app.py\n@@ -1,2 +1,2 @@\n import os\n-import sys, json\n+import sys\n")
        A("add", "fix", "--finding", f["id"], "--title", "t", "--patch-file", self.dir / "fix.diff")
        A("ready", "--note", "This session may not change the repository.")
        page = self.get("api/review")
        self.assertEqual((page["actions"][0]["include"], page["proposal"], page["note"]), (False, "ready", "This session may not change the repository."))
        api({"op": "action-set", "id": page["actions"][0]["id"], "include": True})
        self.assertEqual(self.get("api/review")["findings"][0]["choices"], ["fix"])
        # Rewording the finding drafts its fix again.
        api({"op": "set", "id": f["id"], "text": "one, reworded"})
        self.assertEqual((self.get("api/review")["actions"], self.get("api/review")["proposal"]), ([], "requested"))
        # The finish lists what was drafted and not carried out.
        run("listen", self.dir, "--timeout", 5)
        A("add", "fix", "--finding", f["id"], "--title", "t", "--patch-file", self.dir / "fix.diff")
        self.post("api/finish", {})
        self.assertIn("1 drafted action was not carried out", run("listen", self.dir, "--timeout", 5).stdout)


if __name__ == "__main__":
    unittest.main()
