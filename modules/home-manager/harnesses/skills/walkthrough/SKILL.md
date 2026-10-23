---
name: walkthrough
description: Teach a human a topic, concept, paper, subsystem or codebase as a slideshow in their browser, one idea per slide, with diagrams, quoted code or text, comprehension checks and a question box on every slide that you answer from the session. Use when the user asks to learn, understand or be taught something, or to be walked through how something works; for walking through a code change, use walkthrough-pr.
---

# Walkthrough

Build the reader's understanding of a topic. Present it as a slideshow served from this machine: each slide teaches one idea with prose, excerpts and diagrams, and has its own question box. You stay in the session and answer the questions. Keep the sources and any repository state unchanged throughout.

The bundled script `scripts/deck.py` (Python ≥ 3.11, standard library only) checks the deck, serves the page and passes questions to you. Read `references/deck.md` for the deck format before writing one.

## 1. Establish the topic

Bound the deck before writing it. Settle three things from the request and conversation, and ask for any the request does not give:

- **Goal**: what the reader should be able to do afterwards, such as explain a mechanism, predict a behavior, change a subsystem safely or judge a paper's claim. One sentence; it goes in the deck as `goal`.
- **Starting point**: what the reader already knows. Skip what they know and define what they do not.
- **Sources**: the code, documents, papers or pages the deck draws on. Name them to the reader. Read enough of them to explain the topic, and distinguish what a source states from what you infer.

Set the slide budget from the goal, counting checks but not the appendix: two to four slides for one concept, six to eight for a subsystem or a paper, and rarely more than a dozen. A reader who feels there is too much material skims, and a skimmed deck teaches nothing. When a draft runs over, cut whole slides rather than trimming each one: fold a minor idea into the slide that uses it, and move everything the goal does not need to the appendix or propose it as a follow-up deck.

Create a deck directory in a scratch location outside any repository. Save fetched text you will quote (a paper, a web page, documentation) into it as plain text files.

## 2. Write the deck

Shape the deck around what the reader needs to reach the goal, not around the sources' structure. Order the ideas by conceptual dependency: a slide uses only what earlier slides or the starting point provide. These sections usually work: the problem or motivation, the core ideas one slide each, how they fit together, limits and tradeoffs, and an appendix.

Give each slide one idea and put slide breaks where a reader would pause. Draw a diagram when the idea has a shape that text cannot show well, such as a system's components and how they connect, a data flow, a sequence of messages, or a structure in memory; one such diagram beats three excerpts. Do not draw what a list or table says as well: items, steps or a before-and-after comparison drawn as boxes take longer to read and add nothing. Make a diagram interactive or animated when stepping through it or changing an input teaches something, such as an algorithm's iterations, a request's path through components, or state over time. Keep it static otherwise.

Readers ask better questions when they can point at things. They can click line numbers in excerpts, diagram parts and callouts to quote them into a question, and select any text to quote it. Make slides easy to point at: put each caveat, tradeoff or open question in its own callout, prefer a diagram of labeled parts to a paragraph that describes a system, and give hand-drawn diagram parts labels that make sense on their own.

Quote code or text where it teaches the idea, in small excerpts selected by line range from a file on disk or a file you saved into the deck directory.

Write formulas as TeX math (`$…$`, `$$…$$`; see Math in `references/deck.md`), not in code blocks or plain text.

Check slides guard against skimming. End every deck but a trivial one with a final check of two to four questions that span the goal; `check` fails for a deck of more than three content slides without one. Add a check after a run of slides only when their content is key, conceptual or easy to misread; most slides need none. Ask what the material implies, not what the slide said: predict an output, name the line or step that enforces an invariant, say what happens on an edge case. Give each wrong choice a plausible misreading and a sentence saying what was misread, and point `see` at the slide that settles it. The reader answers with number keys, moves on with Enter, and can skip a check with one keystroke, so a check costs little when it is not needed; a check with weak questions costs trust. Where choices would let the reader guess, ask for a written answer instead, in prose or code, which you judge against a rubric (see Written answers in `references/deck.md`); one or two per deck is enough.

Write `DECK/deck.json`, then run `python3 <skill-dir>/scripts/deck.py check DECK` until it passes. `check` prints the slide list and warns about content slides outside the appendix that no check question points at. Decide for each whether the goal depends on it: if it does, point a question at it; if it does not, consider moving it to the appendix. A warning is not an error.

## 3. Serve it

```sh
python3 <skill-dir>/scripts/deck.py serve DECK --detach
```

This prints a URL with a secret token. The server listens on 127.0.0.1. If the reader's browser runs on another machine, add `--tunnel` (`--tunnel USER@HOST` when the reader reaches this machine under another name): `serve` then also prints the ssh command that forwards the port, and the reader needs both. Alternatively, pass `--host` with an address that machine can reach. If your harness sandboxes commands, the server needs permission to listen on a port.

A deck directory keeps its URL: later servers reuse its token, port and `serve` options. Running `serve` again prints the URL of the running server. If that server runs another copy of `deck.py`, for example one from before an update, or other options, `serve` restarts it in place, and open pages reconnect and reload.

Give the reader the URL, the goal and the number of slides. Tell them they can ask on any slide, quote lines by clicking line numbers (shift-click or drag to extend), and click diagram parts or callouts, or select text, to quote those. Then start listening.

## 4. Answer questions

```sh
python3 <skill-dir>/scripts/deck.py listen DECK --timeout SECONDS
```

`listen` waits for the reader's next question, prints it with its slide and any quoted lines, and marks it as being answered. Investigate as needed. Then write a concise Markdown answer to a file, citing `path:line` or the source's section, and post it while waiting for the next question:

```sh
python3 <skill-dir>/scripts/deck.py listen DECK --answer ID --file ANSWER.md --timeout SECONDS
```

Every `listen` run prints the exact command to run next. Questions and answers pass through files in the deck directory, so `listen` needs no network access.

- Use the longest timeout your harness allows for a command (the default is 100 seconds). If your harness can run a command in the background and notify you when it exits, run `listen` that way, so the user can still talk to you in between. Otherwise run it in the foreground.
- When a run ends without a question, run it again. Stop listening when the reader finishes the walkthrough (`listen` reports it), when the user tells you to stop, or when no page has had the deck open for about 30 minutes. Then tell the user how to resume.
- Answer the question asked. Distinguish what the sources show from what you infer, and do not change the sources.
- If the reader asks for more depth, a diagram, a correction or a new slide, edit `deck.json` and run `check`. The open page reloads in place and keeps the reader's slide. Say in the answer what changed.
- If `listen` hands you a question you already claimed, you lost track of it; answer it now.
- `listen` also reports what the page could not draw: formulas KaTeX rejects, diagrams that fail, text in hand-drawn SVG that is wider than its box, and widget scripts that throw. Fix these in `deck.json` before the reader reaches the slide; mention a fix in your next answer only if the reader saw the slide.
- If `listen` says the server is out of date, run `serve` again to restart it.
- `listen` also hands you written check answers, with the rubric and any test results. Judge the answer against the rubric, not its wording or the tests alone, and post feedback with `--verdict correct`, `partial` or `incorrect`; `listen` prints the command. Say what is right, what is missing and where the deck covers it, and do not give away the model answer.

The page shows the reader whether you are listening, answering or away. Questions asked while you are away wait in the queue.

## 5. Finish

When the reader finishes, `listen` reports how they did on the check slides. Go over what they missed in the conversation, briefly and without scolding: state the point and where in the deck or the sources it lives. Then connect the pieces back to the goal. Name anything you could not read or resolve, and any follow-up deck you proposed. Stop the server with `python3 <skill-dir>/scripts/deck.py stop DECK` (it also exits after 12 idle hours). The questions and answers remain in `DECK/threads.json`, and the check answers in `DECK/quiz.json`.

## Without a browser

If no browser can reach this machine, or the reader prefers to stay in the conversation, present the same deck there. Give the goal and the list of slides, then stop and invite questions before the first slide. After that, present one slide per turn and wait for the reader before moving on. Answer questions without advancing, and honor requests to go deeper, go back, jump, skip or stop. Do not treat elapsed time or a missing response as permission to continue.
