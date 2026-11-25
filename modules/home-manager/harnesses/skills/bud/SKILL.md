---
name: bud
description: "Manage agent sessions as a parent-child tree over tmux with `bud` - launch, list, peek, message, close, resume. Use when asked to run a task in a new session, delegate or spin out work, delegate by default, manage child sessions, or message the parent session."
argument-hint: "[task]"
---

# bud

`bud` runs agent sessions in detached tmux sessions as a parent-child tree and passes messages between them. Use the `bud` CLI (`bud --help`, `bud <cmd> --help`) instead of raw `tmux` or harness commands.

## Modes

- `/bud <task>`, or a request to run a task in a new session: launch one child for it.
- `/bud` with no task, or the user asks to delegate by default: for the rest of this session, answer quick lookups and session questions directly, and launch a child for sustained work (investigating, changing code, drafting, reviewing). Loading this skill for any other reason does not turn this on.

This mode, and defaults the user sets for children (harness, model, effort; pass them as `launch` flags), apply to this session only. Never save them to memory or instructions files.

## Launching

Launch with `bud launch <name> --brief <file|->`. A child that edits files or switches branches in a Git checkout that another session uses gets its own `--worktree <branch>` (or `pr:<N>`); pass `--share` only when every session in the checkout only reads. Relay what `launch` prints: the attach command, and any startup dialog (the user must attach to answer it) or error.

Start the brief with the user's request in their words. Add only the context the child needs to understand it (it has none of this conversation, so resolve references such as "that bug") and the constraints of the checkout you chose (for example, a child launched with `--share` must only read). The child can ask its parent for more. If this session needs the result, ask for it; bud tells the child how to reply.

## Supervising

- Run `bud ls` before answering about which sessions exist or what state they are in; the user closes sessions without telling you. STATE `input` means a dialog or question waits on the user.
- `..` is this session's parent (its nearest live ancestor), for `send` and `peek`.
- Pass `--force` (to `send`, or `resume --fresh` over uncommitted changes) only when the user asked.
- After `close`, report the uncommitted changes and leftover processes it prints; kill nothing the user did not ask you to.
