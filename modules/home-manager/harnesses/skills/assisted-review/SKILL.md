---
name: assisted-review
description: Help a human understand a code change with a high-level summary followed by an interactive, logically ordered walkthrough of diff hunks. Use when the user asks to be walked through a change, commit, branch, or pull request one step at a time; ordinary automated bug reviews do not need this skill.
---

# Assisted review

Build the reader's understanding of the change. Keep the source and Git state unchanged during the walkthrough. Use the conversation itself; no extra tools, generated reports, or session files are required.

## Establish the change

Resolve the requested commit, branch comparison, pull request, or local changes from the user's request and conversation. If the target is materially ambiguous, ask before choosing it. State the scope and comparison base. For a branch, compare against its merge base with the base branch. For local changes, account for staged, unstaged, and untracked files, and distinguish their contents.

Read the diff and enough surrounding code, callers, configuration, and tests to explain its behavior. Use commit messages or the pull request description for stated intent. Distinguish observed behavior, documented intent, and inferred rationale; do not invent the author's reasons. If the change is empty or unavailable, say so rather than constructing a walkthrough.

## Orient the reader

Start with a short summary of the problem, resulting behavior, and overall approach. Give a compact reading itinerary ordered by conceptual dependencies and execution flow rather than filename or commit order. For declarative changes, follow how configuration is composed and takes effect.

Assign every changed hunk to a step. A step covers one hunk or a tightly related group of hunks that teaches one idea. Split large hunks into manageable excerpts; explicitly group mechanical or generated changes. Include tests and supporting configuration where they help explain the behavior. Track what remains so small changes do not disappear from the walkthrough.

Present the overview, itinerary, and first step without a separate approval gate. Adapt depth to the reader's questions and stated familiarity.

## Walk one step at a time

For each step:

1. Name the step and its position in the itinerary. Give repository-relative file paths and line locations, linking to source when supported.
2. Show the relevant diff excerpt, with enough context to see the before and after. Label omissions; keep each excerpt small enough to discuss comfortably.
3. Explain what changed, why it matters, and how it connects to the preceding steps. Use a concrete input or example when it clarifies behavior. Explain meaningful consequences rather than paraphrasing syntax.
4. Briefly name what comes next, invite questions, and end the turn. Wait for the reader before advancing.

Answer questions about the current step without advancing the counter. On “next” or “continue,” move to the next step. Honor requests to go deeper, revisit, jump, skip, stop, or show several steps at once. Record explicit skips separately from explained hunks. Do not treat elapsed time or a missing response as permission to advance.

If you notice a concrete correctness concern, explain it at the relevant step and identify uncertainty. Keep the walkthrough focused on understanding; a request to fix the code is separate work. If the underlying diff changes, refresh the affected steps and coverage before continuing.

## Finish

After the final step, briefly connect the pieces back to the initial summary. Account for all changed hunks as explained, explicitly grouped, or skipped; name any unreadable or otherwise unresolved material. State which checks were actually run or merely inspected. Finishing the walkthrough does not imply the reader approved the change.
