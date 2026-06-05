#!/usr/bin/env bats
# Run: nix shell nixpkgs#bats nixpkgs#jq --command bats \
#   modules/home-manager/profiles/desktop/claude-agents-attach.bats

setup() {
  STUBBIN="$BATS_TEST_TMPDIR/bin"
  mkdir -p "$STUBBIN"

  cat > "$STUBBIN/claude" <<'EOF'
#!/usr/bin/env bash
[ "$1" = "agents" ] && printf '%s' "${AGENTS_JSON:-[]}"
EOF
  # rofi stub: save the menu it was handed, then return the 0-based index of the
  # first line containing $ROFI_PICK (mimics `rofi -dmenu -format i`).
  cat > "$STUBBIN/rofi" <<'EOF'
#!/usr/bin/env bash
cat > "$ROFI_MENU"
awk -v pick="$ROFI_PICK" 'index($0, pick) { print NR - 1; exit }' "$ROFI_MENU"
EOF
  cat > "$STUBBIN/wezterm" <<'EOF'
#!/usr/bin/env bash
echo "$@" >> "$WEZTERM"
EOF
  chmod +x "$STUBBIN/claude" "$STUBBIN/rofi" "$STUBBIN/wezterm"

  export PATH="$STUBBIN:$PATH"
  export WEZTERM="$BATS_TEST_TMPDIR/wezterm"
  export ROFI_MENU="$BATS_TEST_TMPDIR/menu"
  SCRIPT="${BATS_TEST_DIRNAME}/claude-agents-attach.sh"
}

@test "lists only background agents and attaches the chosen one" {
  export AGENTS_JSON='[
    {"sessionId":"id-foo","name":"foo-fix","status":"busy","kind":"background","cwd":"/home/x/projects/foo/.claude/worktrees/zz"},
    {"sessionId":"id-iact","name":"editing","status":"idle","kind":"interactive","cwd":"/home/x/projects/iact"},
    {"sessionId":"id-bar","name":"bar-fix","status":"idle","kind":"background","cwd":"/home/x/projects/bar"}
  ]'
  export ROFI_PICK="bar-fix"
  bash "$SCRIPT"
  run cat "$WEZTERM"
  [[ "$output" == *"claude --resume id-bar"* ]]
  run cat "$ROFI_MENU"
  [[ "$output" == *"foo-fix"* ]]
  [[ "$output" != *"editing"* ]]
}

@test "project column strips the worktree suffix; empty name falls back to project" {
  export AGENTS_JSON='[{"sessionId":"id-foo","name":"","status":"busy","kind":"background","cwd":"/home/x/projects/foo/.claude/worktrees/zz"}]'
  export ROFI_PICK="foo"
  bash "$SCRIPT"
  run cat "$WEZTERM"
  [[ "$output" == *"claude --resume id-foo"* ]]
  run cat "$ROFI_MENU"
  [[ "$output" == *"foo · busy · foo"* ]]
}

@test "empty agent list -> no window opened" {
  export AGENTS_JSON='[]'
  export ROFI_PICK="anything"
  bash "$SCRIPT"
  [ ! -f "$WEZTERM" ]
}

@test "only interactive sessions -> no window opened" {
  export AGENTS_JSON='[{"sessionId":"id-iact","name":"editing","status":"idle","kind":"interactive","cwd":"/home/x/projects/iact"}]'
  export ROFI_PICK="editing"
  bash "$SCRIPT"
  [ ! -f "$WEZTERM" ]
}
