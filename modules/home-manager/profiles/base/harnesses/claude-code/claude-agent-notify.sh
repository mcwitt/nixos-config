#!/usr/bin/env bash
# claude-agent-notify — surface a Claude Code agent event as a dunst popup + sound.
# Invoked by the claude-code Notification (needs-input) and Stop (done) hooks,
# which pass the hook payload JSON on stdin. The audio-cue binary is passed via
# $CLAUDE_AGENT_NOTIFY_SOUND (empty => no sound). Keep the modes and the
# CLAUDE_JOB_DIR background gate in sync with default.nix.
set -euo pipefail

mode="${1:?usage: claude-agent-notify needs-input|done}"

payload="$(cat)"
session_id="$(printf '%s' "$payload" | jq -r '.session_id // empty')"
cwd="$(printf '%s' "$payload" | jq -r '.cwd // empty')"

# "done" fires only for background agent-view sessions (CLAUDE_JOB_DIR is set in
# their environment); foreground turn-ends stay silent.
if [ "$mode" = "done" ] && [ -z "${CLAUDE_JOB_DIR:-}" ]; then
  exit 0
fi

# Label: friendly session name -> project (cwd minus worktree suffix) -> "agent".
label=""
if [ -n "$session_id" ]; then
  label="$(claude agents --json 2>/dev/null \
    | jq -r --arg id "$session_id" \
        'map(select(.sessionId == $id)) | (.[0].name // empty)' 2>/dev/null || true)"
fi
if [ -z "$label" ] && [ -n "$cwd" ]; then
  label="$(basename "${cwd%%/.claude/worktrees/*}")"
fi
[ -n "$label" ] || label="agent"

# Audio cue (non-blocking), if a sound binary was provided and is executable.
sound="${CLAUDE_AGENT_NOTIFY_SOUND:-}"
if [ -n "$sound" ] && [ -x "$sound" ]; then
  "$sound" >/dev/null 2>&1 &
fi

case "$mode" in
  needs-input) notify-send -a claude -u critical "Claude Code · $label" "needs input" ;;
  done) notify-send -a claude -u normal "Claude Code · $label" "completed" ;;
  *)
    echo "claude-agent-notify: unknown mode: $mode" >&2
    exit 2
    ;;
esac
