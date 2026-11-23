#!/usr/bin/env bash
# Tests for design-doc-wc, design-doc-signal and design-doc-open. They use
# stubs in place of bud, tmux and emacsclient, and a throwaway repository;
# they send nothing to a real session and open nothing.
set -euo pipefail

here=$(cd "$(dirname "$0")" && pwd)
tools=$(dirname "$here")
work=$(mktemp -d)
trap 'rm -rf "$work"' EXIT
fails=0

check() { # check <name> <expected> <actual>
  if [ "$2" = "$3" ]; then
    echo "ok   $1"
  else
    echo "FAIL $1: expected '$2', got '$3'"
    fails=$((fails + 1))
  fi
}

# --- design-doc-wc -------------------------------------------------------
# By hand: title line 6, todo line 8, head 4 + 7, Goal 2 + 5, Design 2 + 2 + 1,
# the Decisions headline and subtree 0, Gates 2 + 6.
check "wc: counts outside Decisions and source blocks" \
  "45 words, limit 2000" "$("$tools/design-doc-wc" "$here/fixture.org")"

# --- design-doc-signal ---------------------------------------------------
# The stub answers `show <name> --json` with the pane in $STUB_PANE, and
# appends the arguments of `send` to $STUB_LOG after $STUB_DELAY seconds.
# Its interpreter is this shell's, since a build sandbox has no /usr/bin/env.
printf '#!%s\n' "$BASH" >"$work/bud"
cat >>"$work/bud" <<'EOF'
case "$1" in
  show) printf '{"name": "%s", "pane": "%s"}\n' "$2" "${STUB_PANE:-%1}" ;;
  send) sleep "${STUB_DELAY:-0}"; shift; printf '%s\n' "$*" >>"$STUB_LOG" ;;
esac
EOF
chmod +x "$work/bud"
export DESIGN_DOC_SIGNAL_BUD="$work/bud" STUB_LOG="$work/log" STUB_PANE="%7"

repo="$work/docs"
git init -q "$repo"
git -C "$repo" config user.name test
git -C "$repo" config user.email test@example.invalid
printf '#!/bin/sh\nexec "%s/design-doc-signal"\n' "$tools" >"$repo/.git/hooks/post-commit"
chmod +x "$repo/.git/hooks/post-commit"
mkdir "$repo/proj"

commit() { # commit <subject> <file>...
  local subject=$1
  shift
  git -C "$repo" add -- "$@"
  git -C "$repo" commit -q -m "$subject" -- "$@"
}
sent() { # the log, once a detached send has had time to write it
  sleep "${1:-0.5}"
  cat "$STUB_LOG" 2>/dev/null || true
}
sha() { git -C "$repo" rev-parse --short HEAD; }

cp "$here/fixture.org" "$repo/proj/task.org"
printf 'no head here\n' >"$repo/proj/notes.org"
printf 'not a doc\n' >"$repo/proj/readme.txt"

# 1. A commit from outside tmux signals the owner, and only for the doc.
: >"$STUB_LOG"
(unset TMUX TMUX_PANE; commit "proj/task: first" proj/task.org proj/notes.org proj/readme.txt)
check "signal: a commit outside tmux tells the owner" \
  "fixture-owner design-doc-signal: $(sha) changed proj/task.org: proj/task: first" "$(sent)"

# 2. A commit from another pane signals the owner.
: >"$STUB_LOG"
echo "More." >>"$repo/proj/task.org"
TMUX=x TMUX_PANE="%9" commit "proj/task: from another pane" proj/task.org
check "signal: a commit from another pane tells the owner" \
  "fixture-owner design-doc-signal: $(sha) changed proj/task.org: proj/task: from another pane" "$(sent)"

# 3. A commit from the owner's own agent pane does not signal.
: >"$STUB_LOG"
echo "More still." >>"$repo/proj/task.org"
TMUX=x TMUX_PANE="%7" commit "proj/task: the owner's own" proj/task.org
check "signal: the owner's own commit is silent" "" "$(sent)"

# 4. A slow send does not delay the commit, and still arrives.
: >"$STUB_LOG"
echo "Again." >>"$repo/proj/task.org"
start=$(date +%s%N)
(unset TMUX TMUX_PANE; STUB_DELAY=3 commit "proj/task: slow send" proj/task.org)
ms=$((($(date +%s%N) - start) / 1000000))
check "signal: the commit returns in under a second (took ${ms} ms)" "yes" \
  "$([ "$ms" -lt 1000 ] && echo yes || echo no)"
check "signal: the slow send still arrives" \
  "fixture-owner design-doc-signal: $(sha) changed proj/task.org: proj/task: slow send" "$(sent 3.5)"

# 5. Without bud, the commit still succeeds.
echo "Last." >>"$repo/proj/task.org"
(unset TMUX TMUX_PANE; DESIGN_DOC_SIGNAL_BUD="$work/missing" commit "proj/task: no bud" proj/task.org)
check "signal: a commit succeeds when bud is missing" "proj/task: no bud" \
  "$(git -C "$repo" log -1 --format=%s)"

# --- design-doc-open -----------------------------------------------------
# The stub tmux answers `list-clients` with $STUB_CLIENTS, a variable of a
# session from the file $STUB_ENV/<session>, and logs `new-window`. The stub
# emacsclient answers `-e` with $STUB_SHOWN, or fails when $STUB_NO_SERVER is
# set, and logs every other call.
printf '#!%s\n' "$BASH" >"$work/tmux"
cat >>"$work/tmux" <<'EOF'
case "$1" in
  list-clients) printf '%s\n' "${STUB_CLIENTS:-}" | grep . || true ;;
  show-environment) grep -E "^-?$4(=|\$)" "$STUB_ENV/$3" ;;
  display-message) echo own ;;
  new-window) printf 'tmux %s\n' "$*" >>"$STUB_LOG"; echo 2 ;;
esac
EOF
printf '#!%s\n' "$BASH" >"$work/emacsclient"
cat >>"$work/emacsclient" <<'EOF'
case "$1" in
  -e) [ -z "${STUB_NO_SERVER:-}" ] || exit 1; echo "${STUB_SHOWN:-nil}" ;;
  *) printf 'emacsclient %s\n' "$*" >>"$STUB_LOG" ;;
esac
EOF
chmod +x "$work/tmux" "$work/emacsclient"
mkdir "$work/env"
printf -- '-SSH_CONNECTION\nDISPLAY=:0\n' >"$work/env/at-display"
printf 'SSH_CONNECTION=192.0.2.1 1 192.0.2.2 22\n-DISPLAY\n' >"$work/env/over-ssh"
export DESIGN_DOC_OPEN_TMUX="$work/tmux" DESIGN_DOC_OPEN_EMACSCLIENT="$work/emacsclient"
export STUB_ENV="$work/env"
doc=$(realpath "$repo/proj/task.org")
frame="emacsclient -c -n -d :0 -F ((name . \"design-doc\")) -- $doc"
window="tmux new-window -d -P -F #{window_index} -t own: -n task -- $(command -v env) -u ALTERNATE_EDITOR $work/emacsclient -t -- $doc"

open() { # open <arguments of env>...: what the command says, then what it ran
  : >"$STUB_LOG"
  env "$@" "$tools/design-doc-open" "$doc" | sed -n 2p
  cat "$STUB_LOG"
}
# What a pane says when it was made over SSH, and when it was made at the display.
old_ssh=(-u DISPLAY -u WAYLAND_DISPLAY TMUX=x TMUX_PANE=%7 SSH_CONNECTION=old)
old_display=(-u SSH_CONNECTION -u WAYLAND_DISPLAY TMUX=x TMUX_PANE=%7 DISPLAY=:9)
no_tmux=(-u TMUX -u TMUX_PANE -u WAYLAND_DISPLAY)

check "open: prints the doc's path first" "$doc" \
  "$("$tools/design-doc-open" "$repo/proj/../proj/task.org" | sed -n 1p)"
check "open: at the display, a graphical frame, whatever the pane says" \
  "opened in a new Emacs frame on display :0"$'\n'"$frame" \
  "$(open "${old_ssh[@]}" STUB_CLIENTS=$'100 over-ssh\n200 at-display')"
check "open: over SSH, a background tmux window, whatever the pane says" \
  "opened in window 2 (task) of this tmux session, in the background"$'\n'"$window" \
  "$(open "${old_display[@]}" STUB_CLIENTS=$'300 over-ssh\n200 at-display')"
check "open: in tmux with no client, nothing" \
  "not opened: no tmux client is attached" "$(open "${old_display[@]}" STUB_CLIENTS=)"
check "open: outside tmux at the display, a graphical frame" \
  "opened in a new Emacs frame on display :0"$'\n'"$frame" \
  "$(open "${no_tmux[@]}" -u SSH_CONNECTION DISPLAY=:0)"
check "open: outside tmux over SSH, nothing" \
  "not opened: no local display, and this session is outside tmux" \
  "$(open "${no_tmux[@]}" SSH_CONNECTION=old DISPLAY=:0)"
check "open: without an Emacs server, nothing" \
  "not opened: no Emacs server answers" \
  "$(open "${no_tmux[@]}" -u SSH_CONNECTION DISPLAY=:0 STUB_NO_SERVER=1)"
check "open: a doc that a window already shows is not opened again" \
  "not opened: an Emacs window already shows it" \
  "$(open "${no_tmux[@]}" -u SSH_CONNECTION DISPLAY=:0 STUB_SHOWN=t)"

if [ "$fails" -gt 0 ]; then
  echo "$fails failed"
  exit 1
fi
echo "all passed"
