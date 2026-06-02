#!/usr/bin/env bats
# Run: nix run nixpkgs#bats -- modules/home-manager/profiles/desktop/xmonad/workspace-attention.bats

setup() {
  export XDG_RUNTIME_DIR="$BATS_TEST_TMPDIR/run"
  mkdir -p "$XDG_RUNTIME_DIR"
  STUBBIN="$BATS_TEST_TMPDIR/bin"
  mkdir -p "$STUBBIN"
  cat > "$STUBBIN/xmonadctl" <<'EOF'
#!/usr/bin/env bash
echo "$@" >> "$XDG_RUNTIME_DIR/xmonadctl.calls"
EOF
  chmod +x "$STUBBIN/xmonadctl"
  export PATH="$STUBBIN:$PATH"
  SCRIPT="${BATS_TEST_DIRNAME}/workspace-attention.sh"
  STATE="$XDG_RUNTIME_DIR/workspace-attention"
}

@test "cwd form writes one state file with cwd=/source=/message= content" {
  mkdir -p "$BATS_TEST_TMPDIR/proj"
  run bash "$SCRIPT" --source claude --message "done" "$BATS_TEST_TMPDIR/proj"
  [ "$status" -eq 0 ]
  [ "$(ls -1 "$STATE" | wc -l)" -eq 1 ]
  f="$(ls "$STATE")"
  grep -qx "cwd=$(realpath "$BATS_TEST_TMPDIR/proj")" "$STATE/$f"
  grep -qx "source=claude" "$STATE/$f"
  grep -qx "message=done" "$STATE/$f"
}

@test "repeat call is idempotent (one file) but pokes each time" {
  mkdir -p "$BATS_TEST_TMPDIR/proj"
  bash "$SCRIPT" "$BATS_TEST_TMPDIR/proj"
  bash "$SCRIPT" "$BATS_TEST_TMPDIR/proj"
  [ "$(ls -1 "$STATE" | wc -l)" -eq 1 ]
  [ "$(wc -l < "$XDG_RUNTIME_DIR/xmonadctl.calls")" -eq 2 ]
}

@test "does not recreate a pending file once it has been renamed to .seen" {
  mkdir -p "$BATS_TEST_TMPDIR/proj"
  bash "$SCRIPT" "$BATS_TEST_TMPDIR/proj"
  f="$(ls "$STATE")"
  mv "$STATE/$f" "$STATE/$f.seen"
  bash "$SCRIPT" "$BATS_TEST_TMPDIR/proj"
  [ "$(ls "$STATE")" = "$f.seen" ]
}

@test "--workspace writes tag= content (no cwd attribution)" {
  run bash "$SCRIPT" --workspace scratch
  [ "$status" -eq 0 ]
  f="$(ls "$STATE")"
  grep -qx "tag=scratch" "$STATE/$f"
}

@test "missing XDG_RUNTIME_DIR fails" {
  unset XDG_RUNTIME_DIR
  run bash "$SCRIPT" --workspace x
  [ "$status" -ne 0 ]
}
