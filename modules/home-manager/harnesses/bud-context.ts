// pi's counterpart of the SessionStart hook that runs `bud _context` for
// Claude Code and Codex: remind a session launched with bud, or one that
// launched children, of its place in the tree. The text goes into a system
// prompt section, so it survives compaction; it is refreshed when a session
// starts, resumes or forks, and after compaction, the points where Claude Code
// refreshes it.

import type { ExtensionAPI } from "@earendil-works/pi-coding-agent";
import { spawnSync } from "node:child_process";

const bud = "@bud@";

function budContext(): string | undefined {
  const result = spawnSync(bud, ["_context"], { input: "", encoding: "utf8", timeout: 5_000 });
  if (result.error || result.status !== 0 || !result.stdout.trim()) return undefined;
  try {
    return JSON.parse(result.stdout).hookSpecificOutput?.additionalContext || undefined;
  } catch {
    return undefined;
  }
}

export default function (pi: ExtensionAPI) {
  // bud identifies the caller by its tmux session; outside tmux it has nothing to say.
  if (!process.env.TMUX_PANE) return;

  let context: string | undefined;
  const refresh = () => {
    context = budContext();
  };

  pi.on("session_start", refresh);
  pi.on("session_compact", refresh);

  pi.on("before_agent_start", (event) => {
    if (context) {
      event.systemPromptOptions.sections.bud = context;
    } else {
      delete event.systemPromptOptions.sections.bud;
    }
  });
}
