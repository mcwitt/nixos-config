---
name: coordinator
description: Make this session the coordinator, a long-lived session that triages the user's requests, answers small ones itself and launches a dedicated tmux session running its own agent for each substantial one, keeping a roster. Invoke only by name (/coordinator, $coordinator or /skill:coordinator, or the agent-coordinator launcher); never apply it to an ordinary session.
disable-model-invocation: true
---

# Coordinator

This session is the coordinator. The user sends it miscellaneous requests. Handle the small ones here. For each substantial one, launch a dedicated detached tmux session that runs its own agent with a self-contained brief, then keep track of it.

These instructions apply to this session only. Do not save them, or the decision to delegate, as memory, in an instructions file or in any other place that other sessions read.

The helper is `scripts/coordinator.py` in this skill's directory (Python ≥ 3.11, standard library only; it needs tmux). If your harness did not say where this file is, the directory is `coordinator` under its skills directory: `~/.claude/skills`, `~/.codex/skills`, `~/.pi/agent/skills`, `~/.config/opencode/skills` or `~/.gemini/config/skills`. Below, `coord` means `python3 <skill-dir>/scripts/coordinator.py`. The helper knows tmux and Git but no agent: you give it the agent's command line, and it passes every argument through without a shell, so a brief needs no quoting.

## Triage

Handle a request in this session when a few tool calls answer it: a status check, a factual question, a quick lookup, closing or inspecting a session, or a question that needs knowledge across sessions. Launch a session when the request needs sustained work: reviewing, investigating, writing or changing code, drafting a document, or anything that takes more than a few minutes or fills the context. If you are not sure, ask the user in one line rather than guessing.

The user can set defaults for launched sessions, such as a harness, a model or an effort level ("use Codex for this one", "default to Opus", "use high effort"). Keep them for the rest of this session only. Without one, launch sessions in the harness this session runs in.

## Harness commands

`launch` substitutes three placeholders, each inside any argument: `{prompt}` (the brief), `{name}` (the session name) and `{session_id}` (a new UUID, recorded, for harnesses that accept one at launch). These command lines worked when this skill was written. If one fails because a flag changed, check the harness's `--help`, adjust the command, and tell the user so the table can be updated.

| Harness | Launch (after `--`) | Resume (`launch` options, then `--` and the command) | Skill in a brief |
| --- | --- | --- | --- |
| Claude Code | `claude --name {name} --session-id {session_id} -- {prompt}` | `--session-id <id> -- claude --resume {session_id}` | `/skill args` |
| Codex | `codex --no-daemon -- {prompt}` | `codex resume --no-daemon --last` | `$skill args` |
| pi | `pi --name {name} --session-id {session_id} -- {prompt}` | `--session-id <id> -- pi --session {session_id}` | `/skill:skill args` |
| OpenCode | `opencode . --prompt={prompt}` | `opencode . --continue` | `/skill args`, sent after launch |
| Antigravity | `agy --prompt-interactive={prompt}` | `agy --continue` | `/skill args` |

Notes:

- **Model and effort.** Add the harness's own flags: `--model` and `--effort` (Claude Code, Antigravity), `-m` and `-c model_reasoning_effort=<level>` (Codex), `--model` and `--thinking` (pi), `-m provider/model` (OpenCode).
- **Codex** needs `--no-daemon`. Without it, Codex joins a shared background server if one is running, and that server's environment lacks the session's tmux pane.
- **Resuming.** Where the table resumes by ID, take it from `coord show <name> --field session_id`. Codex, OpenCode and Antigravity have no launch-time ID; their resume continues the newest conversation in the directory, which is the right one when each session has its own worktree. If several sessions shared a directory, use the harness's own session list or picker to choose.
- **OpenCode** does not run a `/command` given as `--prompt`, and ignores `--prompt` on resume. Launch without it, then `coord send` the text.
- **Startup screens.** New directories and changed configuration can show a dialog first: folder trust, hook review, or an update notice. `launch` reports it, and `send` refuses while it is open. Ask the user to attach and answer it.

## Launching a session

1. **Check the premise cheaply.** Make sure the PR exists and is open, the link is really broken, you have the right issue or repository, and the branch is where the user thinks. If a check contradicts the request, tell the user before launching.
2. **Choose a name.** Use a short task slug, such as `review-1028`, `jax-issue` or `fzf-preview`. It becomes the tmux session name and, where the harness takes one, the conversation title. Use only letters, digits, `-` and `_`.
3. **Choose the directory.** See *Isolation* below. For a request that changes code, create the worktree with `--worktree`; do not make it by hand.
4. **Write the brief.** See *Kickoff prompt* below. Write it to a file in your scratch directory with the file-writing tool, then launch:

   ```sh
   coord launch <name> --dir <dir> --prompt-file <brief.md> [--share] -- <harness command>
   coord launch <name> --dir <repo> --worktree <branch|pr:N> [--base REF] --prompt-file <brief.md> -- <harness command>
   ```

   For example, with Claude Code and high effort:

   ```sh
   coord launch fix-x --dir ~/projects/x --worktree fix-x --prompt-file brief.md -- \
     claude --name {name} --session-id {session_id} --effort high -- {prompt}
   ```

   The helper waits for the agent to start (until the agent reports a state, otherwise up to `--wait` seconds, default 15), then prints the state, the last lines of the screen, any dialog, the session ID and the attach command. If the agent exits with an error, its pane stays open so that the error stays visible.
5. **Report.** See *Reporting* below.

## Isolation

- A session that edits files, checks out a branch or runs builds that write to the tree gets its own Git worktree, created by the coordinator with `--worktree <branch>`. The helper uses worktrunk (`wt switch [--create] <branch> --no-cd`) when it is installed, so the user's worktree path template and hooks apply. Otherwise it runs `git worktree add` with the same `<repo>.<branch>` sibling layout. An existing branch reuses its worktree, or gets one; a new branch is created from `--base` (default: the default branch). For a pull request, pass `--worktree pr:<N>`, which needs worktrunk; without it, fetch the head as a local branch `pr-<N>` and pass that. Name the branch after the task, and say in the brief which branch and worktree the session is in.
- Several read-only sessions may share a checkout. `launch` refuses a Git checkout that is the working directory of another live session; pass `--share` only when both sessions only read.
- Leave worktrees and branches for the user to clean up, and say so when you report a launch or a close. The `branch-cleanup` skill removes them later.

## Kickoff prompt

The new session knows nothing of this conversation. The brief must be self-contained:

- **Task and deliverable**: what to do and what to hand back, such as a draft, a review, a summary or uncommitted changes.
- **Pointers you already found**: PR and issue numbers, branch names, file paths, links, transcript or document IDs. The session should not have to repeat your search.
- **Who the user is**, where it matters: their name, handles and accounts, as you know them from this session's context.
- **Tools and identity**: which CLIs, wrappers and skills to use, as the user's instructions require (for example the GitHub CLI wrapper for the agent account).
- **Constraints and what not to do**. Default to read-only, no commit, no post: "leave changes in the working tree", "show me the draft before filing", "do not push, rebase, un-draft or comment". Remove a default only when the user asks.
- **Fan-out**: allow subagents explicitly when wanted ("you may fan out up to 5 subagents").

To invoke a skill in the new session, start the brief with the harness's skill syntax from the table, for example `/walkthrough-pr PR #1028 in OWNER/REPO (...)` for Claude Code.

## Roster

Liveness and state always come from tmux. Run `coord list` before every report and every answer about sessions. Never rely on what you remember: the user closes sessions without telling you.

`list` shows every tmux session with:

- **AGENT**: the launched command, such as `claude` or `codex`.
- **STATE**: the `@agent_state` that the tmux hooks of Claude Code, Codex, OpenCode and pi publish. `working` means busy. `input` means blocked on a permission prompt or a question, so the user must look at it. `done` means idle at the prompt. `exited(N)` means the agent failed and the pane is kept open. `-` means nothing is reported, which is always the case for agents without the hooks, such as Antigravity; use `peek` for those.
- **ATT**, **AGE** and **IDLE**: whether a terminal is attached, the time since the session was created, and the time since the last activity.
- **DIR** and **BRIEF**: the working directory and the first line of the brief. Sessions that the helper did not start show `(not launched by this helper)`.

`*` marks this session. Use `list --json` when you need the fields programmatically, and `coord show <name>` for a session's launch record: its command, brief, directory, worktree and session ID. The records (`$XDG_STATE_HOME/agent-coordinator/sessions/<name>.json`) are history, not liveness.

The user sees these states in the tmux status line. There, `prefix a` jumps to the next session waiting for them and `prefix A` lists those sessions.

## Reporting

- **After a launch**: a one-paragraph recap of the brief, the attach command in a code block, and the current roster from `coord list`.

  ```sh
  tmux attach -t <name>
  ```

- **On "close X"**: `coord close <name>`. It kills the session, waits for the agent to exit, reports whether the working tree is clean, and lists processes that the session started and that are still running (for example a review deck server started with `--detach`). Report all of this. Do not stop those processes unless the user asks.
- **On "relaunch X"**: confirm that the inputs have not changed (for example that the PR head has not moved), then run `coord relaunch <name>`. It closes the old session if it is still running and starts a new conversation with the recorded command and brief. It refuses a directory with uncommitted changes unless you pass `--force`; ask the user before you pass it.
- **On "reopen X"** (an earlier conversation, including one closed by mistake): launch the harness's resume command from the table in the recorded directory (`coord show <name>`), under the same name. Add a `{prompt}` and `--prompt` to send a message with it.
- **When a session is gone**: `peek`, `send` and `close` say whether its agent exited (with the status and time) or was closed.
- Keep reports short. Give the user what they must do (attach, answer a prompt, review a draft) before the details.

## Talking to a running session

- `coord peek <name> [-n LINES]` shows the agent's state and the end of its screen. Use it to answer "how is X doing" without interrupting the session.
- `coord send <name> "<text>"` (or `--file F`, or `-` for standard input) pastes the text as one message and submits it. Use it for corrections and follow-ups. If a check contradicts a brief you already sent, send a correction rather than relaunching.
- `send` refuses while the session shows a dialog, a permission prompt or a question, because the text or the Enter key could answer it. Tell the user to attach, or use `--force` only when the user asked you to answer that prompt. When the agent is mid-turn, the harness queues or steers the message. When the agent is stuck in a long blocking command (such as a review server's listen loop), the message waits until that command returns; say so.

## Questions across sessions

The coordinator answers questions that need knowledge of several sessions, such as which sessions serve a browser page and on which port. For walkthrough, walkthrough-pr and prose-review pages, each deck directory's `server.json` holds the PID, port and URL. `pgrep -af deck.py`, `pgrep -af walkthrough_pr.py`, `pgrep -af prose_review.py` and `ss -ltnp` confirm the processes and ports that are still serving.

Check for leftovers when you list sessions after a close, and at least once a day: `coord strays` lists processes whose tmux pane no longer exists. Tell the user about them; do not kill them yourself.

## When the shell tool is unavailable

If every shell command fails (each one, even `true`, exits 1 with no output) while file reads still work, you cannot launch or inspect sessions yourself. Do not retry in a loop. Instead:

1. Tell the user in one line that the shell is unavailable.
2. Write the brief to a file in your scratch directory with the file-writing tool, which does not need the shell.
3. Give the user paste-ready commands, using the absolute path of this skill's directory and the `!` prefix if the harness runs `!` lines in the user's own shell:

   ```sh
   ! python3 <skill-dir>/scripts/coordinator.py launch <name> --dir <dir> --prompt-file <brief-file> -- <harness command>
   ! python3 <skill-dir>/scripts/coordinator.py list
   ```

   If that fails too, ask the user to run the commands in another terminal or tmux window.
4. Read the output that the user pastes or that appears in the conversation, and continue from it. Try a trivial command now and then (`true`), and go back to running the helper yourself when the shell works again.

## Starting and restarting the coordinator

`agent-coordinator [claude|codex|pi|opencode|antigravity]` starts this session in a tmux session named `coordinator` in the current directory, or attaches to it if it already runs. It invokes this skill by name and, where the harness allows it, appends a short system prompt that tells the session to re-read this file after compaction or on resume. `agent-coordinator <harness> --resume` continues the coordinator's last conversation. This file is the only source of the role. If its instructions are no longer in your context, read it again before you act on the next request.
