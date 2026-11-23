---
name: design-doc
description: Write a design doc with the user before code is written, use it as the brief for implementation, and keep it true while the work proceeds, so that the doc, not the session history, is the record of a task. Use when the user asks for a design doc, a spec or a plan to agree on before implementation, asks to work "design-doc first", or returns to a task that already has a design doc. A one-line change or a small fix does not need this skill.
---

# design-doc

A design doc is one document per task. You and the user write it together before code is written. It is the brief for implementation, and you keep it true while the work proceeds. The user reads the doc, not the session, to learn where the task stands.

Start this skill only when the user asks for it. When a task looks large enough to need a doc and the user did not ask, you may suggest one in a single line.

## Settings

This skill names no file format, no place to keep docs and no tool to launch sessions. Settle each of these, and every other setting, in this order:

1. What the user says in the session.
2. The repository's own convention, where it already keeps design docs.
3. A defaults skill. At the start, look in your list of skills for one named exactly `design-doc-defaults`. If it is there, load it and say so in one line.
4. Ask the user, once.

A defaults skill may set: the layout of the head and of a gate; the format and its comment syntax; where docs live; the length limit and its check; how figures are written and viewed; the session tool; how the turn is passed and any signal on top of the check at the start of a turn; where iteration happens; how a ready doc is opened for the user; where a comment is answered; the summary for public places and its check.

A defaults skill may not change the rules below that are marked **fixed**. If it contradicts one, follow this skill and tell the user.

Whatever is chosen must give five things: you can read and write the doc; each writer's changes can be seen separately (a diff or a revision history); the user can attach a comment to a passage; the doc has a stable link; and the doc can hold diagrams, plots and mathematics that the user can see rendered. Prefer figures kept as text, so that a change to a figure can be read as a change.

## What goes in the doc

**Fixed.** The doc holds what the user cares about. Everything that it does not say is the implementer's choice.

The aim is that the code could be built again from the doc into something that the user would accept. Do not record implementation details that the user does not care about: they are one possible realization, and a later implementer may find a better one.

The level of detail follows the user and the project, from a few paragraphs about an idea to a spec with interfaces and constraints. Infer the level from the request.

The first draft carries about as much detail as the request. With no description, write the outline alone. From one sentence, write a title and a line or two in each part. From several paragraphs, write a nearly complete draft. Do not fill the gaps with requirements or design of your own: leave them open, or record an open question.

## Size

Infer the size from the task, and tell the user which one you took:

1. **No doc.** A short dialog in the session is enough for a one-line change or a small fix.
2. **One doc.** The usual case.
3. **A doc with child docs.** For a project or a major feature whose subtasks are large enough for their own session and doc.

## The first draft

Read what the request points to: the code, existing docs, linked issues. Then write the first draft. Do not hold a dialog in the session first: the user wants to work on the doc, not in the session.

Put your questions in the doc as open questions, and only those that this reading does not answer and whose answers change the shape of the design. The user answers by editing the doc. Do not ask what you can find by reading.

## Structure

**Fixed.** Every doc has these four parts, the head first and the gates last:

- **Head.** The status in one line, the owner session, what needs the user now, and links (parent doc, repository and branch, pull request). The defaults skill fixes the form in which the head names the owner session.
- **Goal.** What this is for, and why.
- **Requirements.** What must be true of the result, and what is out of scope. This is where "what the user cares about" is written down. Each gate checks one or more requirements.
- **Gates.** See below.

Three more parts appear between the requirements and the gates when they have content:

- **Design.** How, to the level of detail that the task calls for.
- **Decisions.** What was decided, why, and what was rejected. This replaces the session history. Record a decision in the same change that carries it out. A change that decides nothing, such as a rewording, gets no entry.
- **Open questions.**

Keep the head current. A user who returns after a day must learn from the head alone where the task stands and what is needed from them.

Keep the doc short enough to read in about ten minutes, and the head in under a minute. The decisions log does not count toward the length. Check the length when you save a change. When the doc is over the limit, shorten it, or move a part into a child doc or a linked file, and tell the user.

## Gates

**Fixed.** A gate is an outcome that can be checked. It states what must be true, its status, and who passes it.

Who passes a gate is one of:

- **agent**: you record the evidence in the doc and continue.
- **user**: you stop and wait for the user. The user can mark any gate this way at any time.

The statuses are `proposed` (not yet agreed), `todo`, `doing`, `done`, `blocked` and `dropped`.

**Fixed.** The user agrees the gates before implementation starts. Write no implementation code before that. After that, add or split a gate only with the user's consent, and do not remove or weaken one yourself.

A gate that is a subtask with its own doc links to that doc, and its status follows that doc.

Change a gate's status in the doc when it changes, not at the end of the session.

## Owners and sessions

**Fixed.** A doc has one owner: the session that wrote it with the user. The owner is the only agent that edits the doc's text. The user is the other writer.

The user may give feedback on any doc, and may hand that to a parent session. A parent then leaves comments in the child's doc or messages its owner, and the owner makes the edits. A parent passes a `user` gate only if the user handed over that gate.

The owner implements a small task itself and delegates a large one. There are two kinds of child session. Keep them apart:

- A **task session** has its own doc, lasts, and works with the user. Its brief is its doc, or the request to write one, with a link to the parent doc. It reports to the owner of the parent doc, which updates that doc where needed. It may delegate in the same way. **Fixed.** Propose a task session and wait for the user to agree before you start it.
- A **worker session** is one that you drive yourself, for example to do parts of an implementation in parallel. Use it as you would without this skill: a brief and a report, no doc and no added step.

For sessions you need three things: to start a session with a brief, to message the parent, and to list the children. The defaults skill says which tool gives them. Without such a tool, write the brief and ask the user to start the session.

## Working on the doc with the user

**Fixed.** Each writer's changes are kept apart, the end of a turn is signalled, and you check for the user's changes at the start of each of your turns.

- Save your own edits as your own change, with a message that says what changed.
- Do not write to the doc while it holds changes that are not yours and are not yet saved as the user's change.
- Remember the last change to the doc that you saw. Every later one that is not yours is the user's. Read it before you do anything else.

The user asks for a change by writing a comment next to the passage, in the format's own comment syntax. Every comment in the doc is a note to you, except one inside a code example.

**Fixed.** Act on each comment, remove it, and list what you did in the message of your change.

Do not answer a question, or a request that you disagree with, in the session. Answer where the defaults skill puts discussion: in the backend's own comment threads where it has them, and otherwise as an entry in the open questions that holds your answer or your proposal. Tell the user in the session only where the answer is.

**Fixed.** The body of the doc holds no discussion. When a thread ends, put its outcome into the text or the decisions, and remove the thread.

## Public places

A design doc can hold private details. Where the code is public and the doc is not, do not link to the doc. Give a short summary of it, and check the summary for private details before you post it.
