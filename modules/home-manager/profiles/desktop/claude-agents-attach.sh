#!/usr/bin/env bash
# claude-agents-attach — pick a background Claude agent via rofi and attach to it
# in a dedicated wezterm window. Bound in xmonad (M-S-g). All parsing stays in
# jq: it emits the display labels and the matching session-id list (same filter,
# same order); rofi returns the selected row index, which indexes the id list.
# The project column strips the worktree suffix, matching claude-agent-notify.
set -euo pipefail

json="$(claude agents --json 2>/dev/null || true)"

mapfile -t ids < <(
  printf '%s' "$json" \
    | jq -r '[.[] | select(.kind == "background")][].sessionId' 2>/dev/null || true
)
[ "${#ids[@]}" -gt 0 ] || exit 0

labels="$(
  printf '%s' "$json" | jq -r '
    [.[] | select(.kind == "background")][]
    | ((.cwd // "") | sub("/\\.claude/worktrees/.*$"; "") | sub(".*/"; "")) as $project
    | ([(.name // ""), $project, "agent"] | map(select(. != "")) | .[0]) as $label
    | "\($label) · \(.status // "?") · \($project)"
    | gsub("[\n\t]"; " ")' 2>/dev/null || true
)"

idx="$(printf '%s' "$labels" | rofi -dmenu -i -p agents -format i || true)"
[ -n "$idx" ] || exit 0
case "$idx" in *[!0-9]*) exit 0 ;; esac # non-numeric (e.g. -1 custom entry)

sid="${ids[$idx]:-}"
[ -n "$sid" ] || exit 0

exec wezterm start -- claude --resume "$sid"
