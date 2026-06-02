#!/usr/bin/env bats

setup() {
  STUBBIN="$BATS_TEST_TMPDIR/bin"
  mkdir -p "$STUBBIN"
  cat > "$STUBBIN/workspace-attention" <<'EOF'
#!/usr/bin/env bash
echo "$@" > "$RECORD"
EOF
  chmod +x "$STUBBIN/workspace-attention"
  export PATH="$STUBBIN:$PATH"
  export RECORD="$BATS_TEST_TMPDIR/record"
  SCRIPT="${BATS_TEST_DIRNAME}/claude-stop-attention.sh"
}

@test "extracts .cwd from stdin JSON and calls the CLI" {
  echo '{"cwd":"/home/x/projects/foo","hook_event_name":"Stop"}' | bash "$SCRIPT"
  run cat "$RECORD"
  [[ "$output" == *"--source claude"* ]]
  [[ "$output" == *"/home/x/projects/foo"* ]]
}

@test "no .cwd -> no call" {
  echo '{"hook_event_name":"Stop"}' | bash "$SCRIPT"
  [ ! -f "$RECORD" ]
}
