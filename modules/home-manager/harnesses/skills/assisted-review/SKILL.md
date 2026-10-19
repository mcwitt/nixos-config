---
name: assisted-review
description: Walk a human through a code change (commit, branch, pull request or local edits) as a slideshow in their browser, with diagrams, diff excerpts and a question box on every slide that you answer from the session. Use when the user asks to be walked through a change or helped to understand it; ordinary automated bug reviews do not need this skill.
---

# Assisted review

Build the reader's understanding of the change. Present it as a slideshow served from this machine: each slide teaches one idea with prose, diff excerpts and diagrams, and has its own question box. You stay in the session and answer the questions. Keep the source and Git state unchanged throughout.

The bundled script `scripts/review_deck.py` (Python ≥ 3.11, standard library only) checks the deck, serves the page and passes questions to you. Read `references/deck.md` for the deck format before writing one.

## 1. Establish the change

Resolve the requested commit, branch comparison, pull request, or local changes from the user's request and conversation. If the target is materially ambiguous, ask before choosing it. For a branch, compare against its merge base with the base branch. For local changes, account for staged, unstaged and untracked files, and keep them apart.

Read the diff and enough surrounding code, callers, configuration and tests to explain its behavior. Use commit messages or the pull request description for stated intent. Distinguish observed behavior, documented intent and inferred rationale; do not invent the author's reasons. If the change is empty or unavailable, say so rather than building a deck.

Create a deck directory in a scratch location outside the repository and write the diff into it. Use these flags: an external diff tool, colors or other prefixes break the parser.

```sh
git diff --no-ext-diff --no-color --src-prefix=a/ --dst-prefix=b/ BASE...HEAD > DECK/change.diff
```

For a single commit, diff it against its parent (`REV^ REV`, or `git show --format= --no-ext-diff --no-color --src-prefix=a/ --dst-prefix=b/ REV` for a root commit). For a pull request, diff its head against the merge base with its base branch. For local changes, write staged changes (`git diff --cached …`), unstaged changes (`git diff …`) and untracked files (`git diff --no-index … /dev/null FILE` for each, run from the repository root; it exits with status 1 when the file has content) to separate files and list them with labels in the deck.

Run `python3 <skill-dir>/scripts/review_deck.py hunks DECK` to list the files, hunks and changed line ranges.

## 2. Write the deck

Shape the deck around what the reader needs to understand, not around files or hunks. Scale it to the change: a few-line fix may need two slides (why and what, perhaps risks), while a large change needs many slides of concepts. These sections usually work, in this order:

- **Why**: the problem or motivation, and what was wrong or missing before.
- **What**: the resulting behavior and the overall approach, at a high level.
- **Key ideas**: the concepts, data structures and algorithms the change relies on, usually over several slides, in order of conceptual dependency and execution flow. Explain each with diagrams, pseudocode, concrete examples and the diff excerpts that implement it.
- **Risks and tradeoffs**: what the approach costs, what it rules out, the alternatives, and what could break.
- **Validation**: how the change was tested or measured, often over several slides: tests, benchmarks, manual checks. Separate checks you ran from ones you only read.
- **Appendix**: the remaining changes, in curated groups with short explanations.

Give each slide one idea and put slide breaks where a reader would pause. Illustrate key or tricky concepts with clean diagrams. Make a diagram interactive or animated when stepping through it or changing an input teaches something, such as an algorithm's iterations, a request's path through components, or state over time. Keep it static otherwise.

Readers ask better questions when they can point at things. They can click to quote diagram parts and callouts into a question, and select any text to quote it. Make slides easy to point at: put each risk, tradeoff or open question in its own callout, prefer a diagram of labeled parts to a paragraph that describes them, and give hand-drawn diagram parts labels that make sense on their own.

Show changed code where it teaches the idea, in small excerpts selected by line range. A hunk can be split across slides, and lines can appear more than once.

Coverage is the one firm rule: every changed line must appear on some slide. Put lines that teach nothing new in appendix groups, such as renames, formatting, generated files, test fixtures or plumbing. Give each group a title and a one- or two-sentence note saying what the changes are and why they are safe to skim. `check` fails while any changed line is missing and prints a block you can paste for each gap.

If you notice a concrete correctness concern, raise it on the relevant slide as a `risk` callout and say how sure you are. Keep the deck about understanding; fixing the code is separate work.

When the change is on GitHub (a commit, a pushed branch or a pull request), set `source_url` to a permalink template at the head commit, `https://github.com/OWNER/REPO/blob/HEAD_SHA/{path}#L{line}-L{end}`, so excerpts and selected lines link to the file in context. Leave it out for local changes.

Write `DECK/deck.json`, then run `python3 <skill-dir>/scripts/review_deck.py check DECK` until it passes.

## 3. Serve it

```sh
python3 <skill-dir>/scripts/review_deck.py serve DECK --detach
```

This prints a URL with a secret token. The server listens on 127.0.0.1. If the reader's browser runs on another machine, either pass `--host` with an address that machine can reach, or have them forward the port (`ssh -L PORT:127.0.0.1:PORT HOST`). If your harness sandboxes commands, the server needs permission to listen on a port.

Give the reader the URL, a one-sentence summary of the change and the number of slides. Tell them they can ask on any slide, quote code by clicking line numbers (shift-click or drag to extend), and click diagram parts or callouts, or select text, to quote those. Then start listening.

## 4. Answer questions

```sh
python3 <skill-dir>/scripts/review_deck.py listen DECK --timeout SECONDS
```

`listen` waits for the reader's next question, prints it with its slide and any quoted lines, and marks it as being answered. Investigate as needed. Then write a concise Markdown answer to a file, citing `path:line`, and post it while waiting for the next question:

```sh
python3 <skill-dir>/scripts/review_deck.py listen DECK --answer ID --file ANSWER.md --timeout SECONDS
```

Every `listen` run prints the exact command to run next. Questions and answers pass through files in the deck directory, so `listen` needs no network access.

- Use the longest timeout your harness allows for a command (the default is 100 seconds). If your harness can run a command in the background and notify you when it exits, run `listen` that way, so the user can still talk to you in between. Otherwise run it in the foreground.
- When a run ends without a question, run it again. Stop listening when the reader finishes the walkthrough (`listen` reports it), when the user tells you to stop, or when no page has had the deck open for about 30 minutes. Then tell the user how to resume.
- Answer the question asked. Distinguish what the code shows from what you infer, and do not change the code.
- If the reader asks for more depth, a diagram, a correction or a new slide, edit `deck.json` and run `check`. The open page reloads in place and keeps the reader's slide. Say in the answer what changed.
- If `listen` hands you a question you already claimed, you lost track of it; answer it now.

The page shows the reader whether you are listening, answering or away. Questions asked while you are away wait in the queue.

## 5. Finish

When the reader finishes, connect the pieces back to the opening summary in the conversation. Name anything you could not read or resolve, and say which checks were actually run and which were only inspected. Finishing the walkthrough does not mean the reader approved the change. Stop the server with `python3 <skill-dir>/scripts/review_deck.py stop DECK` (it also exits after 12 idle hours). The questions and answers remain in `DECK/threads.json`.

## Without a browser

If no browser can reach this machine, or the reader prefers to stay in the conversation, present the same deck there. Give the summary (why and what) and the list of slides, then stop and invite questions before showing any diff. After that, present one slide per turn and wait for the reader before moving on. Answer questions without advancing, and honor requests to go deeper, go back, jump, skip or stop. Do not treat elapsed time or a missing response as permission to continue.
