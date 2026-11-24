---
name: design-doc-defaults
description: One user's settings for the design-doc skill - where design docs live, their format and layout, the length check, figures, editing with the user through collab-edit, the session tool and the summary for public pull requests. The design-doc skill loads this skill; it is not used by itself.
---

# design-doc-defaults

These are the settings that the `design-doc` skill leaves open. What the user says in the session comes first, then the repository's own convention for design docs, then this skill. This skill changes none of the core skill's rules.

## Where docs live

- The docs repository is the private GitHub repository `mcwitt/design-docs`, cloned at `~/projects/design-docs`. If the clone is missing, say so and ask; do not create the repository.
- A doc is `<project>/<task>.org`. `<project>` is the name of the code repository. When there is none, use a name the user gave, or choose a short one. `<task>` is a short slug that you choose; name the owner session the same where you can. Say which names you chose.
- A child doc sits beside its parent. A link between docs is a plain relative link.

## Format

- Org, one sentence per line.
- Start from `template.org` in this skill's directory. Each part of it holds a reminder, in angle brackets, of what the part is for. When the request has no description, the first draft is the template as it stands, with only the head filled in. Otherwise replace each reminder with content, and leave out `Design`, `Decisions` and `Open questions` while they have none.
- The head is a description list directly under the title, with the terms `Status`, `Owner session`, `Needs <user's name>` and `Links`.
- The owner line names the owner session in its first code span: `- Owner session :: ~<session>~`.
- Number requirements `R1`, open questions `Q1` and gates `G1`. Never reuse a number.
- A decision has a date and says what was rejected. Record it in the change that carries it out, and state the decision in that commit's message. A change that decides nothing gets no entry.

## Gates

- A gate is a headline under `Gates`. Its TODO keyword is the status, its text is the number and the outcome, and its tag says who passes it (`agent` or `user`).
- State the outcome in the present tense, so that it can be checked as it stands.
- A body is optional. Use it for a way to check the outcome that is not obvious, for a note, and, when you pass a gate, for what you found.
- The line `#+todo: PROPOSED TODO DOING BLOCKED | DONE DROPPED` under the title sets the six statuses for the file.

## Editing with the user

Edit the doc with the user through the `collab-edit` skill; load it. Start an editing session on the doc when its first draft is committed, and whenever the user wants to work on it with you; open it as that skill says. A comment's question is answered in its thread there.

End the editing session, with the user's consent, before you change gate statuses or report to a parent. Its end is one commit in the docs checkout: give it a message whose subject starts `<project>/<task>:` and states the decisions that it carries, then push. Outside an editing session, edit the doc in the checkout as below.

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

Outside an editing session, find the user's changes in your doc's log since the last commit you saw: `git log <last>..HEAD -- <doc>`. Read it at the start of every turn.

Run `git push` after each of your commits. If it fails, say so and go on.

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
