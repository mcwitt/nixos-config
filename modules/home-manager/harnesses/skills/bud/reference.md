# bud reference

## Worktrees

`--worktree <branch>` makes or reuses a worktree of `--dir` (default: the current directory) with worktrunk (`wt switch`), so the user's worktree path template and hooks apply. An existing branch reuses its worktree, or gets one; a new branch is created from `--base` (default: the default branch). `--worktree pr:<N>` checks out a pull request. Name the branch after the task, and say in the brief which branch and worktree the child is in.

## Startup dialogs

A new directory or a changed configuration can show a dialog first: folder trust, hook review or an update notice. `launch` reports it, and `send` refuses while it is open. Ask the user to attach and answer it. The brief waits behind the dialog, except for an OpenCode brief that starts with a `/command`: `launch` then says to send the brief after the dialog.

## Resume

Claude Code and pi resume by conversation ID. Codex, OpenCode and Antigravity continue the newest conversation in the session's directory, which is the right one when each session has its own worktree; `resume` warns when another session uses the same directory, and then check with `peek` that it continued the right conversation.

After a restart of the tmux server, every session is closed and `bud resume` starts each one again. A parent that bud did not launch records its pane again the next time it runs any `bud` command; until then, `send` to it refuses and says why.

## Questions across sessions

A parent answers questions that need knowledge of several sessions, such as which sessions serve a browser page and on which port. For walkthrough, walkthrough-pr and prose-review pages, each deck directory's `server.json` holds the PID, port and URL. `pgrep -af deck.py`, `pgrep -af walkthrough_pr.py`, `pgrep -af prose_review.py` and `ss -ltnp` confirm the processes and ports that are still serving.

Check for leftovers after a close, and at least once a day: `bud strays` lists processes whose tmux pane no longer exists. Tell the user about them; do not kill them yourself.

## The tree in tmux

`prefix S` opens a picker with the whole tree; Enter switches to the chosen session. In `prefix s`, a child's row shows `↳ <parent>`.

## When the shell tool is unavailable

If every shell command fails (each one, even `true`, exits 1 with no output) while file reads still work, you cannot launch or inspect sessions yourself. Do not retry in a loop. Instead:

1. Tell the user in one line that the shell is unavailable.
2. Write the brief to a file in your scratch directory with the file-writing tool, which does not need the shell.
3. Give the user paste-ready commands, with the `!` prefix if the harness runs `!` lines in the user's own shell:

   ```sh
   ! bud launch <name> --brief <file> --harness <h> [--worktree <branch>]
   ! bud ls
   ```

   If that fails too, ask the user to run the commands in another terminal or tmux window.
4. Read the output that the user pastes and continue from it. Try a trivial command now and then (`true`), and go back to running `bud` yourself when the shell works again.

## Checking a harness by hand

The tests use stub agents. After a harness update, check once against the real CLI, in a scratch directory:

1. `bud launch t --harness <h> --brief <file with "say hi">`: the agent starts with the brief, and `bud peek t` shows the answer.
2. `--skill <s>`: the brief runs skill `<s>` (for OpenCode, the `/command` is typed after start).
3. `bud send t "say bye"`, then `bud close t` and `bud resume t`: the conversation continues.
4. In Claude Code and Codex, `/compact` in a parent: the conversation shows the bud reminder.
