// Publish this OpenCode instance's state to tmux as the pane option
// @agent_state, through the shared agent-tmux-state script (see
// ../tmux-state.nix): input while a permission or question waits on the
// user, working while any session (subagents included) is busy, else done.

import type { Plugin } from "@opencode-ai/plugin";
import { spawnSync } from "node:child_process";
import { isatty } from "node:tty";

const script = "@agentTmuxState@";

export default (async () => {
  // Only the interactive TUI owns the pane. A run without a terminal, such as
  // another agent's `opencode run`, must not report on the pane it was started
  // from. The TUI's server worker shares the process's terminal descriptors.
  if (!process.env.TMUX_PANE || !isatty(0) || !isatty(1)) return {};

  const busy = new Set<string>();
  // Pending permission and question requests, by request ID, to their session.
  const pending = new Map<string, string>();
  let published: string | undefined;

  // Publish synchronously: each call is a quick tmux command, and running
  // them in order means a late working or done never lands after off.
  const run = (state: string) => {
    published = state;
    spawnSync(script, [state], { stdio: "ignore" });
  };

  const publish = () => {
    const state = pending.size > 0 ? "input" : busy.size > 0 ? "working" : "done";
    if (state !== published) run(state);
  };

  // The TUI disposes its instance on exit, then terminates the worker this
  // plugin runs in, so clear the state from both.
  const off = () => {
    if (published !== undefined && published !== "off") run("off");
  };
  process.on("exit", off);

  // Forget a session that is idle or gone, with any request it left pending.
  const settle = (sessionID: string) => {
    busy.delete(sessionID);
    for (const [id, session] of pending) {
      if (session === sessionID) pending.delete(id);
    }
  };

  // No state until the first event: the plugin loads about two seconds before
  // the TUI takes input, and agent-coordinator waits for a state before it
  // types into a new session.

  return {
    event: async ({ event }) => {
      const props = (event as any).properties ?? {};
      switch (event.type) {
        case "server.instance.disposed":
          off();
          return;
        case "session.status":
          if (props.status?.type === "idle") {
            // An aborted session leaves nothing waiting on the user.
            settle(props.sessionID);
          } else {
            busy.add(props.sessionID);
          }
          break;
        case "session.deleted":
          settle(props.sessionID ?? props.info?.id);
          break;
        case "permission.asked":
        case "question.asked":
          pending.set(props.id, props.sessionID);
          break;
        case "permission.replied":
        case "question.replied":
        case "question.rejected":
          pending.delete(props.requestID);
          break;
        default:
          return;
      }
      publish();
    },
  };
}) satisfies Plugin;
