"""Tests for assisted-review's review_deck.py: diff parsing, coverage, the
appendix, findings and proposed actions, over teach-me's deck.py.

Usage: python3 test_review_deck.py <path/to/review_deck.py>
"""

import json
import re
import sys
import unittest
import urllib.error
from pathlib import Path

from deck_testing import (
    CHECK, DIFF, FULL_APPENDIX, DeckDirCase, Script, ServerCase, deck_spec, topic_spec,
)

SCRIPT = Script(sys.argv.pop(1))
run = SCRIPT.run


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


class DeckTest(DeckDirCase):
    @classmethod
    def setUpClass(cls):
        cls.rd = SCRIPT.module()

    def setUp(self):
        super().setUp()
        (self.dir / "change.diff").write_text(DIFF)

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
        (self.dir / "server.json").write_text(json.dumps({**info, "engine": "old", "script": "/old/review_deck.py"}))
        r = run("finding", self.dir, "add", "x", check=False)
        self.assertIn("The server is out of date (it runs /old/review_deck.py)", r.stderr)
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

        for bad in ({"op": "set", "id": mine["id"], "severity": "P9"}, {"op": "set", "id": mine["id"], "action": "comment"},
                    {"op": "set", "id": "f-nope", "action": "ignore"}, {"op": "add", "text": " "}, {"op": "frob", "id": mine["id"]}):
            with self.assertRaises(urllib.error.HTTPError) as cm:
                self.post("api/review", bad)
            self.assertEqual(cm.exception.code, 400)
        r = run("finding", self.dir, "set", "f-nope", "--severity", "P1", check=False)
        self.assertEqual((r.returncode, r.stderr.strip()), (1, "no finding f-nope"))
        self.post("api/review", {"op": "set", "id": mine["id"], "action": "ignore"})
        self.post("api/review", {"op": "remove", "id": added["id"]})
        self.assertEqual([f["id"] for f in self.get("api/review")["findings"]], [mine["id"]])
        self.assertIn("[ignore]", run("finding", self.dir, "list").stdout)

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
                    {"op": "set", "id": mine["id"], "severity": "P0", "action": "comment"}):
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
        old = {"id": "f-1-aa", "text": "old", "severity": "P2", "sources": ["reader"], "refs": [], "slide": "", "at": 0, "action": None}
        (self.dir / "findings.json").write_text(json.dumps({"version": 1, "findings": [old]}))
        run("serve", self.dir, "--detach")
        run("finding", self.dir, "add", "dup", "--review")
        review = json.loads((self.dir / "findings.json").read_text())["findings"][1]["id"]
        self.assertIn("from reader and review", run("finding", self.dir, "merge", "f-1-aa", review).stdout)

    def test_finishing_while_the_review_runs(self):
        run("finding", self.dir, "review", "running")
        self.post("api/finish", {})
        self.assertIn("The background review is still running", run("listen", self.dir, "--timeout", 5).stdout)

    def test_reveal_without_review_findings_is_immediate(self):
        self.assertEqual(self.post("api/review", {"op": "request-reveal"}), {"reveal": "shown"})
        self.assertIn("No questions", run("listen", self.dir, "--timeout", 1).stdout)

    def test_proposed_actions(self):
        (self.dir / "deck.json").write_text(json.dumps(deck_spec(pr="https://github.com/o/r/pull/7")))
        self.wait_for(lambda: self.get("api/deck")["pr"])
        A = lambda *a, **k: run("action", self.dir, *a, **k)
        f = self.post("api/review", {"op": "add", "text": "json is unused", "severity": "P2",
                                     "refs": [{"kind": "lines", "hunk": "src/app.py#1", "lo": 1, "hi": 2}]})["finding"]
        self.post("api/review", {"op": "set", "id": f["id"], "action": "comment"})
        self.assertEqual(self.post("api/review", {"op": "request-proposal"}), {"proposal": "requested"})
        out = run("listen", self.dir, "--timeout", 5).stdout
        self.assertIn("The reader asked for proposed actions.", out)
        self.assertIn(f"  {f['id']} (P2, from reader, at app.py new 2): json is unused [comment]", out)
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
        drafting = self.get("api/review")["actions"]
        with self.assertRaises(urllib.error.HTTPError) as cm:
            self.post("api/review", {"op": "carry-out", "ids": [drafting[0]["id"]]})
        self.assertIn(b"still drafting", cm.exception.read())
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
        self.post("api/review", {"op": "action-set", "id": check["id"], "include": False})
        with self.assertRaises(urllib.error.HTTPError):
            self.post("api/review", {"op": "action-set", "id": comment["id"], "title": "mine now"})
        self.assertEqual(self.get("api/review")["actions"][0]["author"], "reader")

        self.post("api/review", {"op": "carry-out", "ids": [comment["id"], fix["id"]]})
        self.assertEqual([a["state"] for a in self.get("api/review")["actions"]], ["running", "running", "proposed"])
        with self.assertRaises(urllib.error.HTTPError) as cm:
            self.post("api/review", {"op": "carry-out", "ids": [comment["id"]]})
        self.assertIn(b"sent to the session just now", cm.exception.read())
        out = run("listen", self.dir, "--timeout", 5).stdout
        self.assertIn("The reader approved these actions.", out)
        self.assertIn("posts as the reader's account, at src/app.py:2 (new side):\n    > Please drop the unused `json` import.", out)
        self.assertIn("-import sys, json", out)
        self.assertNotIn("grep -rn json", out)
        self.assertIn("on https://github.com/o/r/pull/7 as review comments", out)
        A("result", comment["id"], "--status", "done", "--link", "https://github.com/o/r/pull/7#discussion_r1")
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
        run("action", self.dir, "add", "comment", "--finding", b["id"], "--body-file", self.dir / "body.md")
        run("finding", self.dir, "merge", a["id"], b["id"])
        self.assertEqual(self.get("api/review")["actions"][0]["finding"], a["id"])
        self.post("api/review", {"op": "remove", "id": a["id"]})
        self.assertIsNone(self.get("api/review")["actions"][0]["finding"])


if __name__ == "__main__":
    unittest.main()
