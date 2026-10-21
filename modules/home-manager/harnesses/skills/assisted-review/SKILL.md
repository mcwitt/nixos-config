---
name: assisted-review
description: Walk a human through a code change (commit, branch, pull request or local edits) as a slideshow in their browser, with diagrams, diff excerpts and a question box on every slide that you answer from the session. Use when the user asks to be walked through a change or helped to understand it; ordinary automated bug reviews do not need this skill, and learning a topic or codebase without a change belongs to teach-me.
---

# Assisted review

Build the reader's understanding of the change. This skill extends `teach-me`, installed beside it: read `<skill-dir>/../teach-me/SKILL.md` and `<skill-dir>/../teach-me/references/deck.md`, then `references/diffs.md` here for the diff blocks, the appendix and coverage. The deck engine is shared; run it here as `<skill-dir>/scripts/review_deck.py`. Keep the source and Git state unchanged throughout.

## 1. Establish the change

This replaces the base skill's first step; the change is the topic, and the review decision is the goal.

Resolve the requested commit, branch comparison, pull request, or local changes from the user's request and conversation. If the target is materially ambiguous, ask before choosing it. For a branch, compare against its merge base with the base branch. For local changes, account for staged, unstaged and untracked files, and keep them apart.

Read the diff and enough surrounding code, callers, configuration and tests to explain its behavior. Use commit messages or the pull request description for stated intent. Distinguish observed behavior, documented intent and inferred rationale; do not invent the author's reasons. If the change is empty or unavailable, say so rather than building a deck.

Create a deck directory in a scratch location outside the repository and write the diff into it. Use these flags: an external diff tool, colors or other prefixes break the parser.

```sh
git diff --no-ext-diff --no-color --src-prefix=a/ --dst-prefix=b/ BASE...HEAD > DECK/change.diff
```

For a single commit, diff it against its parent (`REV^ REV`, or `git show --format= --no-ext-diff --no-color --src-prefix=a/ --dst-prefix=b/ REV` for a root commit). For a pull request, diff its head against the merge base with its base branch. For local changes, write staged changes (`git diff --cached …`), unstaged changes (`git diff …`) and untracked files (`git diff --no-index … /dev/null FILE` for each, run from the repository root; it exits with status 1 when the file has content) to separate files and list them with labels in the deck.

Run `python3 <skill-dir>/scripts/review_deck.py hunks DECK` to list the files, hunks and changed line ranges.

## 2. Write the deck

Write the deck as the base skill describes, with these rules for a change.

Budget by the change, counting checks but not the appendix: two to four slides for a small fix, six to eight for a typical feature, and rarely more than a dozen for a large change. When a draft runs over, cut whole slides: merge why and what, fold a key idea the decision does not hinge on into the slide that uses it, and move everything else to the appendix. These sections usually work, in this order:

- **Why**: the problem or motivation, and what was wrong or missing before. For a small change, one slide with what.
- **What**: the resulting behavior and the overall approach, at a high level.
- **Key ideas**: the concepts, data structures and algorithms the reader must grasp to judge the change, one slide each, in order of conceptual dependency and execution flow. Explain each with one diagram or concrete example and the diff excerpt that implements it. Ideas the decision does not hinge on go to the appendix.
- **Risks and tradeoffs**: what the approach costs, what it rules out, the alternatives, and what could break. One `risk` callout per risk.
- **Validation**: how the change was tested or measured. One slide unless a result is surprising. Separate checks you ran from ones you only read.
- **Appendix**: the remaining changes, in curated groups with short explanations.

Show changed code where it teaches the idea, in small diff excerpts selected by line range. A hunk can be split across slides, and lines can appear more than once.

Coverage is the one firm rule: every changed line must appear on some slide. Put lines that teach nothing new in appendix groups, such as renames, formatting, generated files, test fixtures or plumbing. Give each group a title and a one- or two-sentence note saying what the changes are and why they are safe to skim. `check` fails while any changed line is missing and prints a block you can paste for each gap.

If you notice a concrete correctness concern, raise it on the relevant slide as a `risk` callout and say how sure you are. Keep the deck about understanding; fixing the code is separate work.

The final check spans the whole change: how the pieces fit, and what the review decision hinges on. Ask what the code does, not what the slide said.

When the change is on GitHub (a commit, a pushed branch or a pull request), set `source_url` to a permalink template at the head commit, `https://github.com/OWNER/REPO/blob/HEAD_SHA/{path}#L{line}-L{end}`, so excerpts and selected lines link to the file in context. Leave it out for local changes.

Write `DECK/deck.json`, then run `python3 <skill-dir>/scripts/review_deck.py check DECK` until it passes.

## 3. Serve, answer and finish

Serve the deck, answer questions and finish as `teach-me` describes, running `review_deck.py` in place of `deck.py`. Give the reader a one-sentence summary of the change in place of the goal. In answers, cite `path:line` and do not change the code. At the finish, say which tests or validations were actually run and which were only inspected; finishing the walkthrough does not mean the reader approved the change. Without a browser, give the summary (why and what) and the slide list, and invite questions before showing any diff.
