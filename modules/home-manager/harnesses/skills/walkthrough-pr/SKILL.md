---
name: walkthrough-pr
description: Walk a human through a code change (commit, branch, pull request or local edits) as a slideshow in their browser, with diagrams, diff excerpts and a question box on every slide that you answer from the session. Use when the user asks to be walked through a change or helped to understand it; ordinary automated bug reviews do not need this skill, and learning a topic or codebase without a change belongs to walkthrough.
---

# Walkthrough of a code change

Build the reader's understanding of the change. This skill extends `walkthrough`, installed beside it: read `<skill-dir>/../walkthrough/SKILL.md` and `<skill-dir>/../walkthrough/references/deck.md`, then `references/diffs.md` here for the diff blocks, the appendix and coverage. Run its engine as `<skill-dir>/scripts/walkthrough_pr.py`: walkthrough's `deck.py`, which it loads from beside it, with the review layer this skill adds. It takes every `deck.py` command. Keep the source and Git state unchanged throughout, except for fixes the reader approves on the Proposed actions slide (see step 3).

## 1. Establish the change

This replaces the base skill's first step; the change is the topic, and the review decision is the goal.

Resolve the requested commit, branch comparison, pull request, or local changes from the user's request and conversation. If the target is materially ambiguous, ask before choosing it. For a branch, compare against its merge base with the base branch. For local changes, account for staged, unstaged and untracked files, and keep them apart.

Read the diff and enough surrounding code, callers, configuration and tests to explain its behavior. Use commit messages or the pull request description for stated intent. Distinguish observed behavior, documented intent and inferred rationale; do not invent the author's reasons. If the change is empty or unavailable, say so rather than building a deck.

Create a deck directory in a scratch location outside the repository and write the diff into it. Use these flags: an external diff tool, colors or other prefixes break the parser.

```sh
git diff --no-ext-diff --no-color --src-prefix=a/ --dst-prefix=b/ BASE...HEAD > DECK/change.diff
```

For a single commit, diff it against its parent (`REV^ REV`, or `git show --format= --no-ext-diff --no-color --src-prefix=a/ --dst-prefix=b/ REV` for a root commit). For a pull request, diff its head against the merge base with its base branch. For local changes, write staged changes (`git diff --cached …`), unstaged changes (`git diff …`) and untracked files (`git diff --no-index … /dev/null FILE` for each, run from the repository root; it exits with status 1 when the file has content) to separate files and list them with labels in the deck.

Decide what happens when the reader asks for a change, and set `edits` in the deck:

| The change | `edits` |
|---|---|
| Local changes, the user's own unpushed branch, or the user's own draft pull request with no reviews or comments yet | `apply`: make small, clear fixes in the working tree as the reader asks |
| A pull request out of draft or with reviews or comments, or anyone else's change | `propose`: record a finding with Fix on; nothing changes until the reader presses Carry out |

Read a pull request's state with `gh pr view NUMBER --json author,isDraft,reviews,comments`. The page shows the choice under the question box, and the reader can switch it. With `apply` on a branch or pull request, work in the user's checkout of its head and diff the merge base against the working tree (`git diff … $(git merge-base BASE HEAD)`) rather than `BASE...HEAD`, so your edits join the change.

When a diff comes from the working tree (local changes, or any change in `apply` mode), give it the command that writes it and set `repo` to the repository's absolute path, so the deck can follow the code: `{"file": "unstaged.diff", "label": "unstaged", "command": "git diff --no-ext-diff --no-color --src-prefix=a/ --dst-prefix=b/"}`, and write the diff with exactly that command. One command writes all untracked files: `git ls-files -z --others --exclude-standard | xargs -0 -r -n1 git diff --no-index --no-ext-diff --no-color --src-prefix=a/ --dst-prefix=b/ /dev/null`. The server reruns the commands every few seconds and tells you when the code has moved on.

Run `python3 <skill-dir>/scripts/walkthrough_pr.py hunks DECK` to list the files, hunks and changed line ranges.

Then start an independent defect review of the same change in the background, so that it runs while you write the deck and does not share its framing. Use the review the user names; otherwise your harness's own code-review command; otherwise a subagent briefed to find correctness, security and performance defects in the diff and report each with a severity, a place (`path:line`) and a one-paragraph explanation. Never let it change the code. Its findings stay hidden from the reader until the end (see step 3).

## 2. Write the deck

Write the deck as the base skill describes, with these rules for a change.

Budget by the change, counting checks but not the appendix: two to four slides for a small fix, six to eight for a typical feature, and rarely more than a dozen for a large change. When a draft runs over, cut whole slides: merge why and what, fold a key idea the decision does not hinge on into the slide that uses it, and move everything else to the appendix. These sections usually work, in this order:

- **Why**: the problem or motivation, and what was wrong or missing before. For a small change, one slide with what.
- **What**: the resulting behavior and the overall approach, at a high level.
- **Key ideas**: the concepts, data structures and algorithms the reader must grasp to judge the change, one slide each, in order of conceptual dependency and execution flow. Explain each with a concrete example, or a diagram where the idea has a shape, and the diff excerpt that implements it. Ideas the decision does not hinge on go to the appendix.
- **Risks and tradeoffs**: what the approach costs, what it rules out, the alternatives, and what could break. One `risk` callout per risk.
- **Validation**: how the change was tested or measured. One slide unless a result is surprising. Separate checks you ran from ones you only read.
- **Appendix**: the remaining changes, in curated groups with short explanations.

Show changed code where it teaches the idea, in small diff excerpts selected by line range. A hunk can be split across slides, and lines can appear more than once.

Write for a reader who scans before reading:

- Enumerate in lists, not in sentences: a numbered list for steps or ordered changes, a bulleted list for parallel items such as the changes a pull request makes or the files it touches.
- Compare in tables: before and after, alternatives, or options with their costs.
- Lead each list item with the key term or claim in bold when the items are longer than a line.

Keep the reader oriented in time and in the diff:

- Say in words what changed, not only in a diagram or an excerpt. For a fix, say whether it fixes a crash, a wrong result or a slowdown.
- Label every diagram or example of behavior as before or after the change, or show both side by side with the same layout.
- Open a slide that explains unchanged code for context by saying so; otherwise the reader looks for its diff.
- Give each slide an idea that no other slide covers. When two slides would make the same point, merge them.

Coverage is the one firm rule: every changed line must appear on some slide. Put lines that teach nothing new in appendix groups, such as renames, formatting, generated files, test fixtures or plumbing. Give each group a title and a one- or two-sentence note saying what the changes are and why they are safe to skim. `check` fails while any changed line is missing and prints a block you can paste for each gap.

Keep specific defects out of the slides, including ones you notice yourself: record them as review findings (step 3), so the reader judges the change before seeing them. Use `risk` callouts for the approach's tradeoffs and limits. Keep the deck about understanding; step 3 says what to do when the reader asks for a change.

The final check spans the whole change: how the pieces fit, and what the review decision hinges on. Ask what the code does, not what the slide said.

For a pull request, set `pr` to its URL; the reader can then mark findings for review comments.

When the change is on GitHub (a commit, a pushed branch or a pull request), set `source_url` to a permalink template at the head commit, `https://github.com/OWNER/REPO/blob/HEAD_SHA/{path}#L{line}-L{end}`, so excerpts and selected lines link to the file in context. Leave it out for local changes.

Write `DECK/deck.json`, then run `python3 <skill-dir>/scripts/walkthrough_pr.py check DECK` until it passes.

## 3. Serve, answer and finish

Serve the deck, answer questions and finish as `walkthrough` describes, running `walkthrough_pr.py` in place of `deck.py`. Give the reader a one-sentence summary of the change in place of the goal. In answers, cite `path:line`. At the finish, say which tests or validations were actually run and which were only inspected; finishing the walkthrough does not mean the reader approved the change. Without a browser, give the summary (why and what) and the slide list, and invite questions before showing any diff.

Change the code only as `edits` says; `listen` repeats it with each question, since the reader can switch it. With `apply`, make a change the reader asks for when the right fix is small and clear, without committing or pushing unless they ask; mark an existing finding it fixes with `finding DECK set ID --fixed`, bring the deck up to date as below, and only then record a new finding for the fix, at its new place, with `finding DECK add TEXT --at PATH:LINE --fixed`. When the fix is unclear or a design choice, record a finding and ask. With `propose`, leave the code alone and record a finding with `--choose fix`.

Keep the deck true to the code. After you change the code, and whenever `listen` says the code has moved on (the reader's own edits, or a fix from Carry out), run `walkthrough_pr.py refresh DECK`: it writes the diffs again from their commands, runs `check`, names the slides whose code changed, and waits for the server to show the new deck. Fix what `check` reports and run `refresh` again until it passes: show the new lines (edits made during the walkthrough fit an appendix group of their own), drop excerpts of lines that are gone, and reword any slide the change makes wrong. Meanwhile the page tells the reader the deck is catching up, and keeps the old deck until the new one passes. Findings follow their lines into the new deck; `listen` lists the ones whose lines are gone, to mark fixed or place again with `finding DECK set ID --at PATH:LINE`.

The reader collects findings as they go, in a drawer the top bar opens, and turns on Comment (with `pr` set) or Fix for the ones to act on, there or on the Findings slide near the end of the deck. Record a finding when the reader asks you to, with `walkthrough_pr.py finding DECK add TEXT --at PATH:LINE[-END]` (new-side lines; repeat `--at` for several places); when they also ask for a comment or a fix, add `--choose comment` or `--choose fix` (`finding DECK set ID --choose ...` for an existing one) rather than asking them to click. Give every finding a severity, using the reader's when they name one:

- **P0**: blocks the release or major use, such as a security hole, an auth bypass or data loss.
- **P1**: urgent; fix it in the next cycle, such as a concurrency bug, a performance regression or sensitive data in logs.
- **P2**: should be fixed eventually.
- **P3**: nice to have.

When the reader adds a finding without one, `listen` hands it to you; set it with `finding DECK set ID --severity P2`. `finding DECK list` prints the findings with the reader's choices, and `listen` reports them when the reader finishes.

Record the background review's findings, and the defects you noticed, as hidden review findings once the server runs: `finding DECK add TEXT --review --severity P1 --at PATH:LINE`, translating the review's own severities to P0 to P3. Mark the review with `finding DECK review running` while it runs and `finding DECK review done` when everything is recorded. When the reader presses Reveal on the Findings slide, `listen` hands you both lists: merge the findings that report the same problem with `finding DECK merge KEEP OTHER...`, reword a merged finding if neither text covers both, and run `finding DECK reveal`.

The Proposed actions slide follows the Findings slide. As soon as the reader turns on a choice with no draft yet, `listen` hands you the findings with their choices; draft while the walkthrough goes on, with `walkthrough_pr.py action DECK add`:

- **comment**: one review comment for each finding marked Comment, written as it will be posted. When the reader wrote the finding's text, leave out `--body-file`: the comment takes that text as written and posts as the reader. It goes at the finding's first place in a file unless you pass `--at` or `--summary`.
- **fix**: one patch for each finding marked Fix, from the repository root.
- **check**: a test or command that would settle a doubt.

Run `action DECK ready` to show them. If your instructions do not let you post to GitHub or change the repository, run `ready` with `--note` saying so; the slide shows it, so the reader does not expect Carry out to do it. The reader can reword a comment, leave an action out, or click a draft to ask for changes, which you make with `action DECK set ID`. Nothing runs until the reader presses Carry out; then `listen` hands you the approved actions, and you report each with `action DECK result ID --status done` or `failed`. The slide keeps a log of Carry out as it goes; before a step that takes more than a minute, say what it is doing with `action DECK progress ID --note TEXT`. Post the comments you wrote under the GitHub identity you normally use, and the comments the reader wrote or edited under the reader's own identity, which the reader's press authorizes for those comments only. Submit reviews as comments, never as approvals or change requests unless the reader asks, and leave applied fixes uncommitted unless the reader asks otherwise.
