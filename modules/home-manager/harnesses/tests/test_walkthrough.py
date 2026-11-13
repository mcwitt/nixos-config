"""Tests for walkthrough's deck.py on its own: plain excerpts, checks, written
answers and the server.

Usage: python3 test_walkthrough.py <path/to/deck.py>
"""

import json
import os
import sys
import time
import unittest
import urllib.error
import urllib.request

from walkthrough_testing import (
    CHECK_PY, NOTES, DeckDirCase, Script, ServerCase, topic_check, topic_spec, written_check,
)

SCRIPT = Script(sys.argv.pop(1))
run = SCRIPT.run


class TopicDeckTest(DeckDirCase):
    @classmethod
    def setUpClass(cls):
        cls.rd = SCRIPT.module()

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
        self.assertNotIn("coverage", deck.extra)
        ex = deck.slides[1]["blocks"]
        self.assertEqual([b["pieces"][0]["hunk"] for b in ex[1:]], [f"{self.dir / 'notes.txt'}#L1-1", "src/lib.py#L1-2"])
        self.assertIn(repo / "src" / "lib.py", deck.sources)
        payload = self.rd.deck_payload(deck, 1)
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

    def test_slide_errors(self):
        spec = topic_spec()
        spec["slides"] += [
            {"id": "intro", "title": "Again", "blocks": [{"md": "x"}]},
            {"id": "9bad", "title": "", "blocks": []},
            {"id": "ok", "title": "Two", "blocks": [{"md": "a", "callout": "b"}]},
            {"id": "ok2", "title": "Diagram", "blocks": [{"diagram": {"type": "steps", "frames": [{"svg": "<svg/>"}]}}]},
        ]
        self.write(spec)
        with self.assertRaises(self.rd.DeckInvalid) as cm:
            self.rd.load_deck(self.dir)
        text = "\n".join(cm.exception.errors)
        for needle in ("duplicate id", "starting with a letter", "non-empty string", "non-empty list",
                       "exactly one of", "at least two frames"):
            self.assertIn(needle, text)

    def test_check_slides(self):
        self.write(topic_spec())
        self.assertIn("no check slides", run("check", self.dir).stdout)
        spec = topic_spec()
        spec["slides"].append(topic_check())
        self.write(spec)
        deck = self.rd.load_deck(self.dir)
        self.assertEqual([s["id"] for s in deck.checks()], ["final"])
        q = deck.checks()[0]["quiz"][0]
        self.assertEqual([c["correct"] for c in q["choices"]], [True, False])
        self.assertEqual((q["see"], q["choices"][1]["see"]), ("lines", ""))
        self.assertEqual(deck.checks()[0]["blocks"], [])
        self.assertIn("ending with a final check", run("check", self.dir).stdout)
        spec["slides"].insert(1, spec["slides"].pop())
        self.write(spec)
        self.assertIn("last authored slide is not a check", run("check", self.dir).stdout)

    def test_text_prints_the_deck_without_the_key(self):
        spec = topic_spec(reader="Knows files. New to line numbers.")
        spec["slides"][0]["blocks"] += [
            {"callout": "Lines start at 1.", "tone": "risk"},
            {"diagram": {"type": "svg", "alt": "Two boxes", "caption": "Flow",
                         "svg": "<svg><text>read &amp; print</text><g data-ask='the reader loop'><text>loop</text></g></svg>"}},
            {"diagram": {"type": "html", "html": "<div class='w'><b>Step</b> one<style>.w{}</style></div>", "script": "x()"}},
            {"diagram": {"type": "steps", "frames": [{"mermaid": "flowchart LR\n  A --> B", "caption": "Before"},
                                                     {"svg": "<svg><text>after</text></svg>"}]}},
        ]
        spec["slides"].append(topic_check())
        spec["slides"][-1]["quiz"] += written_check()["quiz"][:1]
        spec["slides"].append({"id": "extra", "title": "Extra", "section": "Appendix", "blocks": [{"md": "Beyond the goal."}]})
        self.write(spec)
        self.assertIn("reader: Knows files. New to line numbers.", run("check", self.dir).stdout)
        out = run("text", self.dir).stdout
        for needle in (
            "# Sample topic\n\nGoal: Explain what the notes say.\n\nReader: Knows files. New to line numbers.\n",
            "## Slide 1 of 3 `intro`: Intro\n\nHello.\n\n[Callout: risk] Lines start at 1.",
            "[Diagram (svg)] Flow\nDescription: Two boxes\nLabels: read & print | loop | the reader loop",
            "[Diagram (html)]\nText: Step one",
            "Frame 1: Before\n```mermaid\nflowchart LR\n  A --> B\n```\nFrame 2\nLabels: after",
            "[Excerpt: notes.txt, lines 2–3]\n```\n    2  second line\n    3  third line\n```",
            "**final.1** (choose one) What is on line 2?\n\n1. second line\n2. first line",
            "**final.2** (written answer, prose) Why is line 2 second?",
            "## Appendix (not shown)\n\n- Extra\n",
        ):
            self.assertIn(needle, out)
        # The answers stay out: which choice is right, its feedback, the rubric and the model answer.
        for hidden in ("Line 2.", "That is line 1.", "Says it follows", "Because it comes after", "Key:", "Beyond the goal."):
            self.assertNotIn(hidden, out)
        key = run("text", self.dir, "--key", "--appendix").stdout
        self.assertIn("2. first line\n\nKey: 1. Settled by slide `lines`.", key)
        self.assertIn("Rubric: Says it follows line 1. Settled by slide `lines`.", key)
        self.assertIn("## Slide 4 of 4 `extra` (Appendix): Extra\n\nBeyond the goal.", key)
        # The brief puts the cold reader's instructions and the stated reader before the deck.
        brief = run("text", self.dir, "--brief").stdout
        self.assertTrue(brief.startswith("You are a test reader"))
        self.assertIn("stand in for\n\nKnows files. New to line numbers.\n", brief)
        self.assertTrue(brief.endswith(out))
        blind = run("text", self.dir, "--brief", "--checks-only").stdout
        self.assertIn("without the deck", blind)
        self.assertNotIn("Hello.", blind)
        self.assertIn("must not see the key", run("text", self.dir, "--brief", "--key", check=False).stderr)
        del spec["reader"]
        self.write(spec)
        r = run("text", self.dir, "--brief", check=False)
        self.assertEqual(r.returncode, 1)
        self.assertIn("error: reader: say in deck.json", r.stderr)
        self.write({**spec, "reader": "Knows files. New to line numbers."})
        checks = run("text", self.dir, "--checks-only").stdout
        self.assertTrue(checks.startswith("# Sample topic\n\n## Slide 3 of 3 `final`: Check\n\n**final.1**"))
        for hidden in ("Hello.", "second line\n    3", "Goal:", "Appendix"):
            self.assertNotIn(hidden, checks)

    def test_check_validation(self):
        spec = topic_spec()
        q = topic_check()["quiz"][0]
        spec["slides"] += [
            {"id": "c1", "title": "Both", "blocks": [{"md": "x"}], "quiz": [q]},
            {"id": "c2", "title": "No key", "quiz": [{**q, "choices": [dict(c, correct=False) for c in q["choices"]]}]},
            {"id": "c3", "title": "Two keys", "quiz": [{**q, "choices": [dict(c, correct=True) for c in q["choices"]]}]},
            {"id": "c4", "title": "Self", "quiz": [{**q, "see": "c4"}]},
            {"id": "c5", "title": "Other check", "quiz": [{**q, "see": "final"}, {**q, "see": "nope"}]},
            {"id": "c6", "title": "One choice", "quiz": [{**q, "choices": q["choices"][:1]}]},
            {"id": "c7", "title": "No why", "quiz": [{**q, "choices": [{"text": "a", "correct": True}, {"text": "b"}]}]},
            {"id": "c8", "title": "Empty", "quiz": []},
            topic_check(),
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
        self.assertFalse([e for e in errors if "'final'" in e], errors)

    def test_keys_of_other_scripts_are_errors(self):
        self.write(topic_spec(diffs=["change.diff"], pr="https://github.com/o/r/pull/1"))
        r = run("check", self.dir, check=False)
        self.assertEqual(r.returncode, 1)
        self.assertIn("error: diffs: not a key this script reads", r.stdout)
        self.assertIn("error: pr: not a key this script reads", r.stdout)


class WrittenCheckTest(DeckDirCase):
    @classmethod
    def setUpClass(cls):
        cls.rd = SCRIPT.module()

    def setUp(self):
        super().setUp()
        (self.dir / "tests").mkdir()
        (self.dir / "tests" / "check.py").write_text(CHECK_PY)

    def write(self, quiz):
        spec = topic_spec()
        spec["slides"].append({"id": "final", "title": "Check", "quiz": quiz})
        super().write(spec)

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
        quiz.append({**quiz[1], "test": {**quiz[1]["test"], "dir": "."}})
        quiz.append({"q": "?", "see": "lines", "kind": "essay"})
        (self.dir / "runs" / "t").mkdir(parents=True)
        quiz.append({**quiz[1], "test": {**quiz[1]["test"], "dir": "runs/t"}})
        self.write(quiz)
        with self.assertRaises(self.rd.DeckInvalid) as cm:
            self.rd.load_deck(self.dir)
        errors = "\n".join(cm.exception.errors)
        for expected in ("quiz[0]: a prose question has a rubric, not choices", "quiz[0].rubric: expected",
                         "quiz[1].test.dir:", "quiz[1].test.file: expected a file name",
                         "quiz[1].test.timeout:", "quiz[2].test.dir: expected a subdirectory",
                         "quiz[3].kind: expected choice, prose or code", "quiz[4].test.dir: expected a subdirectory"):
            self.assertIn(expected, errors)


class DialsTest(DeckDirCase):
    """Audience and detail: stops, stored variants and their keys."""

    @classmethod
    def setUpClass(cls):
        cls.rd = SCRIPT.module()

    def variant(self, slide="lines", a="novice", d="1", body=None, check=True):
        f = self.dir / "v.json"
        f.write_text(json.dumps(body or {"title": "Lines, briefly", "blocks": [{"md": "Two lines."}, {"excerpt": "notes.txt", "range": "2"}]}))
        return run("variant", self.dir, slide, "--audience", a, "--detail", d, "--file", f, check=check)

    def test_stops_take_numbers_and_names(self):
        self.assertEqual(self.rd.parse_stop("novice", "audience"), 1)
        self.assertEqual(self.rd.parse_stop(" Complete", "detail"), 5)
        self.assertEqual(self.rd.parse_stop(3, "detail"), 3)
        for bad in ("0", "6", "beginner"):
            with self.assertRaises(ValueError):
                self.rd.parse_stop(bad, "audience")
        self.assertIn("Definitions only of names", self.rd.stop_definition("audience", 4))
        self.assertIn("The point and nothing else", self.rd.stop_definition("detail", 1))

    def test_setting_in_deck_json(self):
        self.write(topic_spec(audience="expert"))
        self.assertEqual(self.rd._setting(self.rd.load_deck(self.dir).spec, []), (5, 3))
        self.write(topic_spec(detail=2))
        self.assertIn("audience: required", run("check", self.dir, check=False).stdout)
        self.write(topic_spec())
        self.assertNotIn("dials", self.rd.deck_payload(self.rd.load_deck(self.dir), 1))

    def test_store_show_and_go_stale(self):
        self.write(topic_spec(audience=3, detail=3, slides=topic_spec()["slides"] + [topic_check()]))
        self.assertIn("Stored lines at audience 1 (novice), detail 1 (concise)", self.variant().stdout)
        deck = self.rd.load_deck(self.dir)
        v = deck.extra["variants"].fresh["lines"]["1-1"]
        self.assertEqual((v["title"], v["section"], v["setting"]), ("Lines, briefly", "", {"audience": 1, "detail": 1}))
        dials = self.rd.deck_payload(deck, 1)["dials"]
        self.assertEqual((dials["audience"], dials["detail"], dials["slides"]), (3, 3, ["intro", "lines"]))
        self.assertIn('"Two lines."', run("variant", self.dir, "lines", "--show", "-a", "1", "-d", "concise").stdout)
        self.assertIn("lines: 1-1", run("variant", self.dir).stdout)
        text = run("text", self.dir, "-a", "1", "-d", "1")
        self.assertIn("Lines, briefly", text.stdout)
        self.assertIn("shown as written: intro", text.stderr)
        # Editing the slide as written, or the lines it quotes, retires the variant.
        (self.dir / "notes.txt").write_text(NOTES.replace("second line", "2nd line"))
        deck = self.rd.load_deck(self.dir)
        self.assertEqual((deck.extra["variants"].fresh, len(deck.extra["variants"].stale)), ({}, 1))
        self.assertIn("1-1*", run("variant", self.dir).stdout)
        run("variant", self.dir, "--prune")
        self.assertFalse((self.dir / "variants" / "lines" / "1-1.json").exists())

    def test_invalid_variants_are_refused(self):
        self.write(topic_spec(audience=3, detail=3, slides=topic_spec()["slides"] + [topic_check()]))
        r = self.variant(body={"blocks": [{"excerpt": "missing.txt"}]}, check=False)
        self.assertEqual(r.returncode, 1)
        self.assertIn("cannot read", r.stdout)
        self.assertFalse((self.dir / "variants" / "lines" / "1-1.json").exists())
        self.assertIn("not a content slide", self.variant(slide="final", check=False).stderr)
        self.assertIn("deck's own setting", self.variant(a="3", d="3", check=False).stderr)
        self.assertIn("expected 1-5", self.variant(a="7", check=False).stderr)
        # A broken file on disk leaves the deck itself valid.
        bad = self.dir / "variants" / "intro" / "2-2.json"
        bad.parent.mkdir(parents=True)
        bad.write_text("{")
        self.assertIn("variants/intro/2-2.json", run("variant", self.dir).stdout)
        run("check", self.dir)

    def test_cold_read_brief_states_the_setting(self):
        self.write(topic_spec(audience="newcomer", detail="concise", reader="Knows: Python. New to: the notes."))
        brief = run("text", self.dir, "--brief").stdout
        self.assertIn("- Audience newcomer:", brief)
        self.assertIn("- Detail concise:", brief)


class DialsServerTest(ServerCase):
    script = SCRIPT

    def spec(self):
        return {**topic_spec(audience=4, detail="standard"), "slides": [*topic_spec()["slides"], topic_check()]}

    def test_request_listen_store_and_ask(self):
        with self.assertRaises(urllib.error.HTTPError) as cm:
            self.post("api/variant", {"slides": ["final"], "audience": 1, "detail": 3})
        self.assertEqual(cm.exception.code, 400)
        r = self.post("api/variant", {"slides": ["intro", "lines"], "audience": "novice", "detail": 3})
        self.assertEqual(r["queued"], ["intro", "lines"])
        # The deck's own setting and a request already waiting queue nothing.
        self.assertEqual(self.post("api/variant", {"slides": ["lines"], "audience": 4, "detail": 3})["queued"], [])
        self.assertEqual(self.post("api/variant", {"slides": ["lines"], "audience": 1, "detail": 3})["queued"], [])
        out = run("listen", self.dir, "--timeout", 5).stdout
        self.assertIn("asked for 2 slides at another setting", out)
        self.assertIn("- lines (slide 2 of 3", out)
        self.assertIn("audience 1: novice:", out)
        self.assertIn(" variant ", out)
        self.wait_for(lambda: self.get("api/deck")["dials"]["requests"].get("lines", {}).get("1-3") == "claimed")
        f = self.dir / "v.json"
        for sid in ("intro", "lines"):
            f.write_text(json.dumps({"blocks": [{"md": f"{sid} for a novice."}]}))
            run("variant", self.dir, sid, "-a", "1", "-d", "3", "--file", f)
        dials = self.wait_for(lambda: (d := self.get("api/deck")["dials"])["variants"].get("lines") and not d["requests"] and d)
        self.assertEqual(dials["variants"]["lines"]["1-3"]["blocks"][0]["md"], "lines for a novice.")
        self.assertEqual(list((self.dir / "queue").glob("q-*")), [])
        # A stored setting needs no request; a question says what the reader saw.
        self.assertEqual(self.post("api/variant", {"slides": ["lines"], "audience": 1, "detail": 3})["queued"], [])
        msg = self.post("api/ask", {"slide": "lines", "text": "Why?", "view": "1-3"})["message"]
        self.assertEqual(msg["view"], "1-3")
        out = run("listen", self.dir, "--timeout", 5).stdout
        self.assertIn("sees this slide at audience 1 (novice), detail 3 (standard)", out)
        self.assertIn("variants/lines/1-3.json", out)


class CoreServerTest(ServerCase):
    script = SCRIPT

    def spec(self):
        return {**topic_spec(), "slides": [*topic_spec()["slides"], topic_check()]}

    def quiz(self):
        return self.get("api/quiz").get("final", {})

    def test_round_trip(self):
        root = self.url.rsplit("/", 2)[0]
        with self.assertRaises(urllib.error.HTTPError) as cm:
            urllib.request.urlopen(root + "/wrong-token/api/deck", timeout=5)
        self.assertEqual(cm.exception.code, 404)
        page = self.page()
        self.assertIn(b"deck.js", page)
        self.assertNotIn(b"<!-- extensions -->", page)
        self.assertNotIn(b"review.js", page)
        for path in ("review.js", "api/review"):
            with self.assertRaises(urllib.error.HTTPError):
                self.page(path)

        deck = self.get("api/deck")
        self.assertEqual((deck["title"], deck["status"]), ("Sample topic", {"session": "away"}))
        self.assertNotIn("coverage", deck)
        msg = self.post("api/ask", {"slide": "lines", "text": "Why?", "refs": [
            {"kind": "quote", "source": "callout", "text": "**Risky:** the `json` import"},
            {"kind": "quote", "source": "elsewhere", "text": "a box"},
            {"kind": "quote", "source": "math", "text": "$x_1 + a_{ij}$"},
            {"kind": "quote", "source": "finding", "text": "not a source here"},
            {"kind": "lines", "hunk": "nope#1", "lo": 0, "hi": 0}]})["message"]
        self.assertEqual([r["source"] for r in msg["refs"]], ["callout", "diagram", "math", "diagram"])
        out = run("listen", self.dir, "--timeout", 5).stdout
        self.assertIn(f"Question {msg['id']} on slide lines", out)
        self.assertIn("Quoted from a callout:\n> **Risky:** the `json` import", out)
        self.assertIn("Quoted from a diagram:\n> a box", out)
        self.assertIn("Quoted from a formula:\n> $x_1 + a_{ij}$", out)
        self.wait_for(lambda: self.get("api/threads")["lines"]["messages"][0]["state"] == "claimed")
        answer = self.dir / "answer.md"
        answer.write_text("Because.")
        self.assertIn("Posted the answer", run("listen", self.dir, "--answer", msg["id"], "--file", answer, "--timeout", 1).stdout)
        msgs = self.wait_for(lambda: len(self.get("api/threads")["lines"]["messages"]) == 2 and self.get("api/threads")["lines"]["messages"])
        self.assertEqual((msgs[1]["text"], msgs[0]["state"]), ("Because.", "answered"))
        self.assertEqual(list((self.dir / "queue").glob("q-*")), [])

        second = self.post("api/ask", {"slide": "intro", "text": "Never mind"})["message"]
        self.post("api/withdraw", {"id": second["id"]})
        self.assertEqual(self.get("api/threads")["intro"]["messages"][0]["state"], "withdrawn")
        self.post("api/clear", {"slide": "intro"})
        self.assertNotIn("intro", self.get("api/threads"))
        with self.assertRaises(urllib.error.HTTPError) as cm:
            self.post("api/ask", {"slide": "intro", "text": "x"}, {"Origin": "http://evil.example"})
        self.assertEqual(cm.exception.code, 403)
        self.post("api/finish", {})
        self.assertIn("finished", run("listen", self.dir, "--timeout", 5).stdout)

    def test_check_answers(self):
        self.assertEqual(self.get("api/quiz"), {})
        for bad in ({"slide": "lines", "question": 0, "choice": 0}, {"slide": "final", "question": 1, "choice": 0},
                    {"slide": "final", "question": 0, "choice": 2}, {"slide": "final", "question": "0", "choice": 0}):
            with self.assertRaises(urllib.error.HTTPError) as cm:
                self.post("api/answer", bad)
            self.assertEqual(cm.exception.code, 400)
        self.post("api/answer", {"slide": "final", "question": 0, "choice": 1})
        self.assertEqual(self.get("api/quiz"), {"final": {"0": 1}})
        self.assertEqual(json.loads((self.dir / "quiz.json").read_text())["answers"], {"final": {"0": 1}})
        self.post("api/finish", {})
        out = run("listen", self.dir, "--timeout", 5).stdout
        self.assertIn("Checks: 0 of 1 answered correctly.", out)
        self.assertIn("Check, question 1: What is on line 2?", out)
        self.assertIn("chose: first line", out)
        self.assertIn("correct: second line", out)
        self.assertIn("covered on slide 2 of 3: Lines", out)
        self.post("api/answer", {"slide": "final", "question": 0, "choice": None})
        self.assertEqual(self.get("api/quiz"), {"final": {}})
        self.post("api/answer", {"slide": "final", "question": 0, "choice": 0})
        self.post("api/finish", {})
        out = run("listen", self.dir, "--timeout", 5).stdout
        self.assertIn("Checks: 1 of 1 answered correctly.", out)
        self.assertNotIn("chose:", out)

    def test_deck_edits_reload(self):
        self.write({**self.spec(), "title": "Renamed"})
        self.wait_for(lambda: self.get("api/deck")["title"] == "Renamed")
        version = self.get("api/deck")["version"]
        self.write({**self.spec(), "slides": []})
        time.sleep(2.5)
        deck = self.get("api/deck")
        self.assertEqual((deck["title"], deck["version"]), ("Renamed", version))

    def test_restart_keeps_url(self):
        self.assertEqual(run("serve", self.dir, "--detach").stdout, f"already serving:\n{self.url}\n")
        run("stop", self.dir)
        self.assertIn("received SIGTERM; shutting down", (self.dir / "server.log").read_text())
        self.assertEqual(run("serve", self.dir, "--detach").stdout.strip(), self.url)
        self.assertEqual(self.get("api/deck")["title"], "Sample topic")
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
        with self.assertRaises(ProcessLookupError):
            os.kill(info["pid"], 0)
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
        issue = {"slide": "lines", "where": "diagram 1", "message": "Mermaid: Parse error on line 3"}
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
        self.assertIn("  slide 2 of 3 (lines), diagram 1: Mermaid: Parse error on line 3\n", out)
        self.assertNotIn("nope", out)
        self.assertIn("No questions", run("listen", self.dir, "--timeout", 1).stdout)

    def test_listen_times_out_and_reports_presence(self):
        out = run("listen", self.dir, "--timeout", 1).stdout
        self.assertIn("No questions in the last 1 s", out)
        self.assertIn("listen", out.splitlines()[-1])

    def test_quoted_plain_lines_reach_listen(self):
        msg = self.post("api/ask", {"slide": "lines", "text": "Why?", "refs": [{"kind": "lines", "hunk": "notes.txt#L2-3", "lo": 1, "hi": 1}]})["message"]
        self.assertEqual(msg["refs"][0]["label"], "notes.txt 3")
        out = run("listen", self.dir, "--timeout", 5).stdout
        self.assertIn("Selected lines, notes.txt 3:\n```text\n    3  third line\n```", out)
        (self.dir / "a.md").write_text("Because.")
        run("listen", self.dir, "--answer", msg["id"], "--file", self.dir / "a.md", "--timeout", 1)
        self.assertNotIn("coverage", self.get("api/deck"))

    def test_written_answers_are_judged_by_the_session(self):
        (self.dir / "tests").mkdir()
        (self.dir / "tests" / "check.py").write_text(CHECK_PY)
        spec = topic_spec()
        spec["slides"].append(written_check())
        self.write(spec)
        self.wait_for(lambda: '"prose"' in json.dumps(self.get("api/deck")))

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

        # A judgment nobody finishes stops blocking the question.
        claim = self.dir / "queue" / f"{prose['id']}.claimed"
        os.utime(claim, (time.time() - 3600, time.time() - 3600))
        self.post("api/answer", {"slide": "final", "question": 0, "text": "It follows $L_1$."})
        prose = self.quiz()["0"]
        self.assertEqual(prose["state"], "queued")
        run("listen", self.dir, "--timeout", 5)

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
