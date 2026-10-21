// Publish this pi session's state to tmux as the pane option @agent_state,
// through the shared agent-tmux-state script (see ../tmux-state.nix):
// working from the start of a run until pi settles, else done. pi asks for no
// approvals itself, so it never reports input.

import type { ExtensionAPI } from "@earendil-works/pi-coding-agent";
import { spawnSync } from "node:child_process";

const script = "@agentTmuxState@";

export default function (pi: ExtensionAPI) {
  if (!process.env.TMUX_PANE) return;

  let published: string | undefined;

  // Publish synchronously: each call is a quick tmux command, and running
  // them in order means a late working or done never lands after off.
  const publish = (state: string) => {
    if (state === published) return;
    published = state;
    spawnSync(script, [state], { stdio: "ignore" });
  };

  // Only the interactive TUI owns the pane. A print, JSON or RPC run, such as
  // another agent's `pi -p`, must not report on the pane it was started from.
  pi.on("session_start", (_event, ctx) => {
    if (ctx.mode === "tui") publish("done");
  });
  pi.on("agent_start", (_event, ctx) => {
    if (ctx.mode === "tui") publish("working");
  });
  // Not agent_end: pi may still retry, compact or run queued messages.
  pi.on("agent_settled", (_event, ctx) => {
    if (ctx.mode === "tui") publish("done");
  });
  // Also fires when /new or /resume replaces the session; the next
  // session_start publishes done again.
  pi.on("session_shutdown", () => {
    if (published !== undefined) publish("off");
  });

  // pi can exit without a session_shutdown, as on a crash.
  process.on("exit", () => {
    if (published !== undefined) publish("off");
  });
}
