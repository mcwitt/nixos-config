#!/usr/bin/env bats
# Run: nix shell nixpkgs#bats nixpkgs#jq --command bats \
#   modules/home-manager/profiles/base/harnesses/claude-code/claude-agent-notify.bats

setup() {
  STUBBIN="$BATS_TEST_TMPDIR/bin"
  mkdir -p "$STUBBIN"

  cat > "$STUBBIN/notify-send" <<'EOF'
#!/usr/bin/env bash
echo "$@" >> "$NOTIFY"
EOF
  cat > "$STUBBIN/sound-stub" <<'EOF'
#!/usr/bin/env bash
echo played >> "$SOUND"
EOF
  cat > "$STUBBIN/claude" <<'EOF'
#!/usr/bin/env bash
[ "$1" = "agents" ] && printf '%s' "${AGENTS_JSON:-[]}"
EOF
  chmod +x "$STUBBIN/notify-send" "$STUBBIN/sound-stub" "$STUBBIN/claude"

  export PATH="$STUBBIN:$PATH"
  export NOTIFY="$BATS_TEST_TMPDIR/notify"
  export SOUND="$BATS_TEST_TMPDIR/sound"
  export CLAUDE_AGENT_NOTIFY_SOUND="$STUBBIN/sound-stub"
  SCRIPT="${BATS_TEST_DIRNAME}/claude-agent-notify.sh"
}

@test "needs-input: friendly name from json, critical urgency, plays sound" {
  export AGENTS_JSON='[{"sessionId":"abc","name":"flaky-fix","cwd":"/x"}]'
  echo '{"session_id":"abc","cwd":"/home/x/projects/foo/.claude/worktrees/zz"}' \
    | bash "$SCRIPT" needs-input
  sleep 0.3
  run cat "$NOTIFY"
  [[ "$output" == *"-u critical"* ]]
  [[ "$output" == *"needs input · flaky-fix"* ]]
  [ -f "$SOUND" ]
}

@test "done (background): falls back to project basename when no name" {
  export CLAUDE_JOB_DIR="$BATS_TEST_TMPDIR"
  export AGENTS_JSON='[]'
  echo '{"session_id":"zzz","cwd":"/home/x/projects/foo/.claude/worktrees/zz"}' \
    | bash "$SCRIPT" done
  run cat "$NOTIFY"
  [[ "$output" == *"-u normal"* ]]
  [[ "$output" == *"done · foo"* ]]
}

@test "done (foreground): no CLAUDE_JOB_DIR -> no notification" {
  unset CLAUDE_JOB_DIR
  echo '{"session_id":"zzz","cwd":"/home/x/projects/foo"}' | bash "$SCRIPT" done
  [ ! -f "$NOTIFY" ]
}

@test "label falls back to 'agent' when name and cwd are absent" {
  export CLAUDE_JOB_DIR="$BATS_TEST_TMPDIR"
  export AGENTS_JSON='[]'
  echo '{"session_id":"zzz"}' | bash "$SCRIPT" done
  run cat "$NOTIFY"
  [[ "$output" == *"done · agent"* ]]
}
