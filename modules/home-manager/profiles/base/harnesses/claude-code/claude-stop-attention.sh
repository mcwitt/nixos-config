#!/usr/bin/env bash
# Claude Code Stop hook: when the top-level agent finishes a turn, flag the
# workspace that owns the session's cwd. Reads the hook payload JSON on stdin.
set -euo pipefail

cwd="$(jq -r '.cwd // empty')"
if [ -n "$cwd" ]; then
  exec workspace-attention --source claude --message "agent finished" "$cwd"
fi
