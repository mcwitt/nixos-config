---
name: bud
description: Launch a task in a new agent session (its own tmux session, a child of this one) and supervise this session's children - list, peek, message, close, resume. Use when the user asks to do something "in a new session", to spin out, spin off, delegate or hand off a task, or to "bud" one; when the user asks this session to delegate substantial requests by default; when asked about, or to message, close or resume, a session this one launched; and when this session must report to the session that launched it.
argument-hint: "[task]"
---

# bud

bud runs agent sessions in detached tmux sessions, as a tree. The session that launches a child becomes its parent. Any session can be a parent, at any depth. The helper is the `bud` command. It knows each harness's flags, so you never write a harness command line yourself.

These instructions, triage mode and any defaults the user sets for launched sessions apply to this session only. Do not save them, or the decision to delegate, as memory, in an instructions file or in any other place that other sessions read.

## The model

- **Session**: a tmux session that runs one agent. Its tmux name is its identity.
- **Parent and child**: a child's record names the session that launched it. A session without a parent is a root.
- **Effective parent**: the nearest live ancestor. When a parent closes or dies, its children report to the next live session above it.
- **Records** under `~/.local/state/bud/sessions/` hold the tree and the history. tmux alone says what is live. Run `bud ls` before every report and every answer about sessions. Never rely on what you remember: the user closes sessions without telling you.

## How you were invoked

- `/bud <task>` (or the user asked for a new session in other words): launch one child for the task, then report.
- `/bud` with no task, typed by the user: turn on **triage mode** for the rest of this session. From then on, handle a request in this session when a few tool calls answer it: a status check, a factual question, a quick lookup, closing or inspecting a session, or a question that needs knowledge across sessions. Launch a child when the request needs sustained work: reviewing, investigating, writing or changing code, drafting a document, or anything that takes more than a few minutes or fills the context. If you are not sure, ask the user in one line.
- Loaded by yourself, to supervise or report: do not turn on triage mode.

The user can set defaults for launched sessions, such as a harness, a model or an effort level ("use Codex for this one", "default to Opus", "use high effort"). Keep them for this session only. Without one, a child uses this session's harness and the model you run on.

## Launching a child

1. **Check the premise cheaply.** Make sure the PR exists and is open, the link is really broken, you have the right issue or repository, and the branch is where the user thinks. If a check contradicts the request, tell the user before launching.
2. **Choose a name**: a short task slug, such as `review-1028`, `jax-issue` or `fzf-preview`, with letters, digits, `-` and `_`. It becomes the tmux session name and, where the harness takes one, the conversation title.
3. **Choose the directory.** A child that edits files, checks out a branch or runs builds that write to the tree gets its own worktree: pass `--worktree <branch>` (or `--worktree pr:<N>`), and do not make the worktree by hand. Read-only children may share a checkout: `launch` refuses a checkout that another live session uses unless you pass `--share`, which you pass only when both sessions only read.
4. **Write the brief** to a file in your scratch directory with the file-writing tool (see *The brief*). Then launch:

   ```sh
   bud launch <name> --brief <file> [--dir <dir>] [--worktree <branch> [--base <ref>]] [--share] \
     [--harness <h>] [--model <m>] [--effort <e>] [--skill <skill>] [-- <extra harness args>]
   ```

   The first time this session launches a child, pass `--harness` with the harness you run in (`claude`, `codex`, `pi`, `opencode` or `agy`). `--skill S` starts the brief with skill S in the child harness's own syntax. bud adds a last line to the brief that tells the child who its parent is and how to message it.

   `launch` waits for the agent to start, then prints the state, the end of the screen, any startup dialog, the session ID and the attach command. If the agent exits with an error, its pane stays open so that the error stays visible.
5. **Report**: a one-paragraph recap of the brief, the attach command in a code block (`tmux attach -t <name>`), and the tree from `bud ls`. Say that worktrees and branches are left for the user to clean up; the `branch-cleanup` skill removes them later.

## The brief

The new session knows nothing of this conversation. The brief must be self-contained:

- **Task and deliverable**: what to do and what to hand back, such as a draft, a review, a summary or uncommitted changes.
- **Pointers you already found**: PR and issue numbers, branch names, file paths, links, transcript or document IDs. The child should not have to repeat your search.
- **Who the user is**, where it matters: their name, handles and accounts, as you know them from this session's context.
- **Tools and identity**: which CLIs, wrappers and skills to use, as the user's instructions require (for example the GitHub CLI wrapper for the agent account).
- **Constraints and what not to do**. Default to read-only, no commit, no post: "leave changes in the working tree", "show me the draft before filing", "do not push, rebase, un-draft or comment". Remove a default only when the user asks. A skill that asks the user's approval for each action keeps it.
- **Reporting back**, when the parent needs the result: "when done, report with `bud send .. "<summary>"`".
- **Fan-out**: allow subagents explicitly when wanted ("you may fan out up to 5 subagents").

## Supervising children

- `bud ls` shows this session's subtree, indented, with HARNESS, STATE, ATT, AGE, IDLE, DIR and BRIEF. STATE `working` means busy; `input` means a permission prompt or question waits on the user; `done` means idle at the prompt; `exited(N)` means the agent failed and its pane stays open; `-` means the harness reports no state (use `peek`). `--all` adds every tmux session, `--closed` adds history, `--json` gives fields.
- `bud peek <name> [-n LINES]` shows the state and the end of the screen, the input box included. Use it to answer "how is X doing" without interrupting.
- `bud send <name> "<text>"` (or `--file F`, or `-`) pastes the text as one message, starting `[from <this session>]`, and submits it. Use it for corrections and follow-ups; if a check contradicts a brief you already sent, send a correction rather than relaunching. `send` refuses while the target shows a dialog, a permission prompt or a question, because the text or Enter could answer it: tell the user to attach. When someone is typing in the target, `send` waits for 15 seconds without a key press before it submits; if the typing goes on for 2 minutes, it pastes without Enter and the user submits. Pass `--force` only when the user asked you to answer that prompt.
- `bud send .. "<text>"` messages this session's own parent. `bud peek ..` looks at it.
- `bud close <name>` kills the session, waits for the agent to exit, reports whether the working tree is clean and lists processes that the session started and that are still running. Report all of this, and do not stop those processes unless the user asks. Its children report to the next live session above it; `close --tree` closes them too.
- `bud resume <name>` starts a closed session again and continues its conversation. `bud resume <name> --fresh` starts a new conversation from the same brief; confirm first that the inputs have not changed (for example that the PR head has not moved), and pass `--force` for a directory with uncommitted changes only when the user agrees.
- `bud show <name>` prints a session's record: brief, directory, worktree, session ID.

Keep reports short. Give the user what they must do (attach, answer a prompt, review a draft) before the details.

For worktrees and pull requests, startup dialogs, resume details, questions across sessions and the fallback when the shell does not work, read `reference.md` in this skill's directory.
