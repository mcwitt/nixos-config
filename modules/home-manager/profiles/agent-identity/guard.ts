// Run each agent bash command through agent-gh-guard (./guard.sh), the same
// check Claude Code and Codex run as a PreToolUse hook: a command that calls
// `gh` directly is blocked, with the reason sent back to the model. The script
// takes Claude Code's hook input on stdin and exits 2 to block.

import { type ExtensionAPI, isToolCallEventType } from "@earendil-works/pi-coding-agent";
import { spawnSync } from "node:child_process";

const guard = "@guard@";

export default function (pi: ExtensionAPI) {
  pi.on("tool_call", (event) => {
    if (!isToolCallEventType("bash", event)) return undefined;

    const result = spawnSync(guard, [], {
      input: JSON.stringify({ tool_input: { command: event.input.command } }),
      encoding: "utf8",
      timeout: 10_000,
    });

    // Fail closed when the guard cannot run at all: that is a broken install,
    // not a command the guard judged.
    if (result.error) {
      return { block: true, reason: `agent-gh-guard could not run: ${result.error.message}` };
    }
    if (result.status === 2) {
      return { block: true, reason: result.stderr.trim() };
    }
    return undefined;
  });
}
