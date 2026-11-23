---
name: design-doc-defaults
description: One user's settings for the design-doc skill - where design docs live, their format and layout, the length check, figures, how a doc is opened, the session tool, the commit signal and the summary for public pull requests. The design-doc skill loads this skill; it is not used by itself.
---

# design-doc-defaults

These are the settings that the `design-doc` skill leaves open. What the user says in the session comes first, then the repository's own convention for design docs, then this skill. This skill changes none of the core skill's rules.

## Where docs live

- The docs repository is the private GitHub repository `mcwitt/design-docs`, cloned at `~/projects/design-docs`. If the clone is missing, say so and ask; do not create the repository.
- A doc is `<project>/<task>.org`. `<project>` is the name of the code repository, or a name the user gives when there is none. `<task>` is a short slug; name the owner session the same where you can.
- A child doc sits beside its parent. A link between docs is a plain relative link.

## Format

- Org, one sentence per line. Start from `template.org` in this skill's directory.
- The head is a description list directly under the title, with the terms `Status`, `Owner session`, `Needs <user's name>` and `Links`.
- A program reads the owner line, so keep its form: the session's name is the first code span on the line that starts `- Owner session ::`.
- Number requirements `R1`, open questions `Q1` and gates `G1`. Never reuse a number.
- A decision has a date and says what was rejected. Record it in the one commit that carries it out, and state the decision in that commit's subject. A commit that decides nothing gets no entry.

## Gates

- A gate is a headline under `Gates`. Its TODO keyword is the status, its text is the number and the outcome, and its tag says who passes it (`agent` or `user`).
- State the outcome in the present tense, so that it can be checked as it stands.
- A body is optional. Use it for a way to check the outcome that is not obvious, for a note, and, when you pass a gate, for what you found.
- The line `#+todo: PROPOSED TODO DOING BLOCKED | DONE DROPPED` under the title sets the six statuses for the file.

## Comments from the user

- A comment is an Org comment line (`# ...`) on the line above the sentence it is about. Find them with `grep -nE '^[[:space:]]*# ' <doc>`.
- A `# ` line inside a source or example block is part of the block, not a comment to you.

## Length

Run `design-doc-wc <doc>` before each commit. It prints the number of words outside the `Decisions` subtree and outside source blocks, and the limit.

## Figures

- Mathematics is LaTeX in the text: `\(...\)` and `\[...\]`.
- A diagram is a Mermaid source block. A plot is a source block in the language of its script; put a long script in a file beside the image, and call it from the block.
- Each block writes an image to `<project>/<task>/`, as SVG where it can.
- Evaluate the blocks you change and commit the images with the doc, so that the user sees every figure on opening the doc.

## Working in the docs checkout

Every doc is committed on `main`, in the one checkout. Other sessions and the user work in the same checkout, so keep these rules:

1. Write only your own doc and its figure directory.
2. Commit only those paths: `git commit -m "<subject>" -- <paths>`. Never run a command that acts on the whole tree, such as `git add -A`, `git commit -a`, `git stash`, `git reset` or `git checkout .`.
3. Start each commit subject with `<project>/<task>:`.

To find the user's changes, read your doc's log since the last commit you saw: `git log <last>..HEAD -- <doc>`.

Run `git push` after each of your commits. If it fails, say so and go on.

## Opening the doc

When the first draft is committed, run `design-doc-open <doc>` and tell the user what it printed. The first line is the doc's path. The second says what was done: the doc is in a new Emacs frame on the user's display, or in a background window of this tmux session, or it was not opened and why. Run the command again only when the user asks.

## The commit signal

The checkout has a `post-commit` hook that runs `design-doc-signal`. If `.git/hooks/post-commit` is missing, create it with these two lines and make it executable:

```sh
#!/bin/sh
exec design-doc-signal
```

After a commit that changes your doc and that you did not make, you get a message that starts `design-doc-signal:` and names the doc, the commit and its subject. Then read your doc's log, as above. The message can be lost (your session shows a prompt, or is closed), so still check the log at the start of every turn.

## Sessions

The session tool is bud; read the `bud` skill before you use it.

- Launch a session with a brief: `bud launch <task> --brief <file>`, with `--worktree <branch>` when the session will change code.
- Message the parent: `bud send .. "<text>"`.
- List the children: `bud ls`.

The brief of a task session links the parent doc, names the doc's path, and allows commits and pushes in the docs repository.

## Pull requests

Find out whether the repository is public: `gh repo view --json visibility -q .visibility`.

- **Private:** give the doc's path in the pull request's description.
- **Public:** give a summary in place of the doc, written in Markdown, with no link to the doc, no path and no session name. The summary has the goal, the requirements, the main decisions, and the gates with their statuses.

Before you post a summary:

1. Read it for private repositories, people, hosts, employers and their projects.
2. Write it to a file and run `grep -inFf ~/projects/design-docs/.private-terms <file>`. Any line it prints holds a private term; remove the term and run it again. The terms file has one term per line and no blank lines. If it is missing, tell the user that this check did not run.
3. Show the summary to the user, unless they have already said to open the pull request.
