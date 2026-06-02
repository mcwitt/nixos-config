#!/usr/bin/env bash
# workspace-attention — mark the workspace owning a directory (or an explicit
# tag) as needing attention, for the xmonad agent-attention indicator.
#
# The consumer is xmonad.hs: it reads $XDG_RUNTIME_DIR/workspace-attention/ and
# renders/clears bells. This script only RECORDS attention and WAKES xmonad; it
# does not compute a workspace tag (xmonad attributes the cwd) and does not fire
# any popup (xmonad owns that). Keep the "workspace-attention" subdir name and
# the "agent-attention-refresh" command in sync with xmonad.hs.
#
# Usage: workspace-attention [--source S] [--message M] [--workspace TAG] [DIR]
set -euo pipefail

source="agent"
message="needs attention"
tag=""
dir=""

while [ $# -gt 0 ]; do
  case "$1" in
    --source) source="$2"; shift 2 ;;
    --message) message="$2"; shift 2 ;;
    --workspace) tag="$2"; shift 2 ;;
    --) shift; break ;;
    -*) echo "workspace-attention: unknown option: $1" >&2; exit 2 ;;
    *) dir="$1"; shift ;;
  esac
done

state_dir="${XDG_RUNTIME_DIR:?XDG_RUNTIME_DIR is not set}/workspace-attention"
mkdir -p "$state_dir"

if [ -n "$tag" ]; then
  ident="tag=$tag"
else
  cwd="$(realpath -- "${dir:-$PWD}")"
  ident="cwd=$cwd"
fi

key="$(printf '%s' "$ident" | sha1sum | cut -d' ' -f1)"
pending="$state_dir/$key"
seen="$state_dir/$key.seen"

# Create only if this attention episode is not already recorded (pending or
# already-notified). Atomic write via temp + rename.
if [ ! -e "$pending" ] && [ ! -e "$seen" ]; then
  tmp="$(mktemp "$state_dir/.tmp.XXXXXX")"
  printf '%s\nsource=%s\nmessage=%s\n' "$ident" "$source" "$message" > "$tmp"
  mv -f "$tmp" "$pending"
fi

# Wake xmonad to re-render now. Harmless no-op if xmonad is absent/restarting.
xmonadctl agent-attention-refresh >/dev/null 2>&1 || true
