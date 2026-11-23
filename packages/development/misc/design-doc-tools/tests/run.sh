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
# The stub tmux names this session "own" and logs `new-window`. The stub
# emacsclient answers `-e` with $STUB_SHOWN, or fails when $STUB_NO_SERVER is
# set.
printf '#!%s\n' "$BASH" >"$work/tmux"
cat >>"$work/tmux" <<'EOF'
case "$1" in
  display-message) echo own ;;
  new-window) printf 'tmux %s\n' "$*" >>"$STUB_LOG"; echo 2 ;;
esac
EOF
printf '#!%s\n' "$BASH" >"$work/emacsclient"
cat >>"$work/emacsclient" <<'EOF'
case "$1" in
  -e) [ -z "${STUB_NO_SERVER:-}" ] || exit 1; echo "${STUB_SHOWN:-nil}" ;;
esac
EOF
chmod +x "$work/tmux" "$work/emacsclient"
export DESIGN_DOC_OPEN_TMUX="$work/tmux" DESIGN_DOC_OPEN_EMACSCLIENT="$work/emacsclient"
doc=$(realpath "$repo/proj/task.org")
window="tmux new-window -d -P -F #{window_index} -t own: -n task -- $(command -v env) -u ALTERNATE_EDITOR $work/emacsclient -t -- $doc"

open() { # open <arguments of env>...: what the command says, then what it ran
  : >"$STUB_LOG"
  env "$@" "$tools/design-doc-open" "$doc" | sed -n 2p
  cat "$STUB_LOG"
}
in_tmux=(TMUX=x TMUX_PANE=%7)

check "open: prints the doc's path first" "$doc" \
  "$("$tools/design-doc-open" "$repo/proj/../proj/task.org" | sed -n 1p)"
check "open: in tmux, a terminal frame in a background window of the session" \
  "opened in window 2 (task) of this tmux session, in the background"$'\n'"$window" \
  "$(open "${in_tmux[@]}")"
check "open: outside tmux, nothing" \
  "not opened: this session is outside tmux" "$(open -u TMUX -u TMUX_PANE)"
check "open: without an Emacs server, nothing" \
  "not opened: no Emacs server answers" "$(open "${in_tmux[@]}" STUB_NO_SERVER=1)"
check "open: a doc that a window already shows is not opened again" \
  "not opened: an Emacs window already shows it" "$(open "${in_tmux[@]}" STUB_SHOWN=t)"

if [ "$fails" -gt 0 ]; then
  echo "$fails failed"
  exit 1
fi
echo "all passed"
