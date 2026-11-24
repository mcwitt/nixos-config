#!/usr/bin/env bash
# Tests for collab-edit. They use stubs in place of bud, tmux and
# emacsclient, and throwaway directories; they send nothing to a real
# session and open nothing.
# The stubs' bodies are single-quoted on purpose.
# shellcheck disable=SC2016
set -euo pipefail

here=$(cd "$(dirname "$0")" && pwd)
tools=$(dirname "$here")
work=$(mktemp -d)
trap 'rm -rf "$work"' EXIT
fails=0
export PATH="$tools:$PATH" COLLAB_EDIT_STATE="$work/state"
export GIT_AUTHOR_NAME=test GIT_AUTHOR_EMAIL=test@example.invalid
export GIT_COMMITTER_NAME=test GIT_COMMITTER_EMAIL=test@example.invalid
unset TMUX TMUX_PANE
# The user's own Git configuration stays out, as in the build sandbox.
export GIT_CONFIG_GLOBAL=/dev/null GIT_CONFIG_NOSYSTEM=1

check() { # check <name> <expected> <actual>
  if [ "$2" = "$3" ]; then
    echo "ok   $1"
  else
    echo "FAIL $1: expected '$2', got '$3'"
    fails=$((fails + 1))
  fi
}

# The stubs' interpreter is this shell's, since a build sandbox has no
# /usr/bin/env. bud answers `show` with the pane in $STUB_PANE and logs
# `send`. tmux names this session "own", makes window @5, and logs
# `new-window` and `kill-window`. emacsclient answers `-e` with
# $STUB_ANSWER, or fails when $STUB_NO_SERVER is set, and logs the form.
stub() { # stub <name> <body>
  printf '#!%s\n%s\n' "$BASH" "$2" >"$work/bin/$1"
  chmod +x "$work/bin/$1"
}
mkdir "$work/bin"
stub bud 'case "$1" in
  show) printf "{\"name\": \"%s\", \"pane\": \"%s\"}\n" "$2" "${STUB_PANE:-%7}" ;;
  send) shift; printf "bud %s\n" "$*" >>"$STUB_LOG" ;;
esac'
stub tmux 'case "$1" in
  display-message) case "$*" in *session_id*) echo "\$1" ;; *) echo own ;; esac ;;
  new-window) echo "tmux new-window" >>"$STUB_LOG"; echo "@5 2" ;;
  list-windows) echo "@5" ;;
  kill-window) echo "tmux kill-window $3" >>"$STUB_LOG" ;;
esac'
stub emacsclient 'case "$1" in
  -e) [ -z "${STUB_NO_SERVER:-}" ] || exit 1; echo "emacs $2" >>"$STUB_LOG"; echo "${STUB_ANSWER:-nil}" ;;
esac'
export COLLAB_EDIT_BUD="$work/bin/bud" COLLAB_EDIT_TMUX="$work/bin/tmux" \
  COLLAB_EDIT_EMACSCLIENT="$work/bin/emacsclient" STUB_LOG="$work/log"

log() { # the log, once a detached send has had time to write it
  sleep "${1:-0.5}"
  cat "$STUB_LOG" 2>/dev/null || true
}
agent() { TMUX=x TMUX_PANE=%7 "$@"; }
user() { TMUX=x TMUX_PANE=%9 "$@"; }
edit() { # edit <dir> <file> <sed script>: change a file and commit it
  sed -i "$3" "$1/$2"
  git -C "$1" commit -q -am "turn: $3"
}

# --- a file outside version control --------------------------------------

mkdir -p "$work/notes/post"
printf '#+title: Post\n\nFirst line.\nSecond line.\n' >"$work/notes/post.org"
printf 'figure\n' >"$work/notes/post/fig.svg"
printf 'a sibling\n' >"$work/notes/other.org"

out=$(agent collab-edit start --session ed "$work/notes/post.org")
dir=$(sed -n 's/^session //p' <<<"$out")
check "start: prints the copy's path first" "$dir/post.org" "$(head -1 <<<"$out")"
check "start: the figure directory is copied, not linked" "no" \
  "$([ -L "$dir/post" ] && echo yes || echo no)"
check "start: a sibling is a link, outside Git" "$work/notes/other.org" \
  "$(readlink "$dir/other.org")"
check "start: the session's repository is clean" "" "$(git -C "$dir" status --porcelain)"
check "start: the session's name is in its configuration" "ed" \
  "$(git -C "$dir" config collab-edit.session)"
check "start: a second session on the file is refused" "refused" \
  "$(agent collab-edit start "$work/notes/post.org" >/dev/null 2>&1 && echo started || echo refused)"
check "status: by the original file" "$dir" \
  "$(collab-edit status "$work/notes/post.org" | head -1)"

: >"$STUB_LOG"
check "open: in tmux, a background window" \
  "opened in window 2 (post) of this tmux session, in the background" \
  "$(agent collab-edit open "$dir" | sed -n 2p)"
check "open: the window is recorded" "@5" "$(git -C "$dir" config collab-edit.window)"

# G8: a commit from Magit wakes the agent, and the agent's own does not.
: >"$STUB_LOG"
(cd "$dir" && GIT_LITERAL_PATHSPECS=1 edit "$dir" post.org 's/First/The first/')
check "signal: a commit with literal pathspecs, outside tmux, tells the agent" \
  "bud ed collab-edit: $(git -C "$dir" rev-parse --short HEAD) in $dir: turn: s/First/The first/" "$(log)"
: >"$STUB_LOG"
user edit "$dir" post.org 's/Second/The second/'
check "signal: a commit from another pane tells the agent" "1" "$(log | grep -c '^bud ed')"
: >"$STUB_LOG"
agent edit "$dir" post.org 's/^\(#+title.*\)$/\1\n# agent: a question/'
check "signal: the agent's own commit is silent" "" "$(log)"
(COLLAB_EDIT_BUD="$work/missing" edit "$dir" post.org 's/The first/First/')
check "signal: a commit succeeds without bud" "turn: s/The first/First/" \
  "$(git -C "$dir" log -1 --format=%s)"

# end refuses while a comment remains, or a change is not committed.
check "end: refuses while a comment remains" "refused" \
  "$(agent collab-edit end "$dir" >/dev/null 2>&1 && echo ended || echo refused)"
agent edit "$dir" post.org '/^# agent:/d'
echo "loose" >>"$dir/post.org"
check "end: refuses an uncommitted change" "refused" \
  "$(agent collab-edit end "$dir" >/dev/null 2>&1 && echo ended || echo refused)"
git -C "$dir" checkout -q -- post.org
check "end: refuses while Emacs holds unsaved changes" "refused" \
  "$(STUB_ANSWER=t agent collab-edit end "$dir" >/dev/null 2>&1 && echo ended || echo refused)"

# G3: the end changes the file in place, and leaves nothing behind.
git -C "$dir" rm -q post/fig.svg
mkdir -p "$dir/post"
mkdir -p "$dir/post"
printf 'new\n' >"$dir/post/new.svg"
git -C "$dir" add post/new.svg
git -C "$dir" commit -q --no-verify -m "turn: figures"
: >"$STUB_LOG"
agent collab-edit end "$dir" >/dev/null
check "end: the file is changed in place" \
  "$(printf '#+title: Post\n\nFirst line.\nThe second line.')" "$(cat "$work/notes/post.org")"
check "end: a figure removed in the session is removed" "no" \
  "$([ -e "$work/notes/post/fig.svg" ] && echo yes || echo no)"
check "end: a figure added in the session is added" "new" "$(cat "$work/notes/post/new.svg")"
check "end: the session's repository is gone" "no" "$([ -e "$dir" ] && echo yes || echo no)"
check "end: the window is closed" "1" "$(grep -c '^tmux kill-window @5$' "$STUB_LOG")"
check "end: the buffer is killed" "1" "$(grep -c 'kill-buffer' "$STUB_LOG")"

# --- a file in a repository ----------------------------------------------

repo="$work/repo"
git init -q "$repo"
mkdir "$repo/docs"
printf '# Guide\n\nOne.\nx\nTwo.\nx\nThree.\n' >"$repo/docs/guide.md"
printf 'other\n' >"$repo/docs/other.md"
git -C "$repo" add -A
git -C "$repo" commit -q -m init
before=$(git -C "$repo" rev-parse HEAD)

dir=$(agent collab-edit start --session ed "$repo/docs/guide.md" | sed -n 's/^session //p')
user edit "$dir" guide.md 's/One/Uno/'
agent edit "$dir" guide.md 's/Three/Tres/'
# Meanwhile the original changes, on a line the session left alone, and an
# unrelated file is left uncommitted.
sed -i 's/Two/Dos/' "$repo/docs/guide.md"
echo "dirty" >>"$repo/docs/other.md"
check "end: in a repository, a message is required" "refused" \
  "$(agent collab-edit end "$dir" >/dev/null 2>&1 && echo ended || echo refused)"
agent collab-edit end "$dir" -m "guide: Spanish numbers" >/dev/null
check "end: one commit on the working branch" "$before" "$(git -C "$repo" rev-parse HEAD~1)"
check "end: the commit has the given message" "guide: Spanish numbers" \
  "$(git -C "$repo" log -1 --format=%s)"
check "end: the commit holds only the document" "docs/guide.md" \
  "$(git -C "$repo" show --name-only --format= HEAD)"
check "end: a change made to the original meanwhile is kept" \
  "$(printf '# Guide\n\nUno.\nx\nDos.\nx\nTres.')" "$(cat "$repo/docs/guide.md")"
check "end: an unrelated change stays uncommitted" " M docs/other.md" \
  "$(git -C "$repo" status --porcelain)"

# A conflict with the original is resolved in the session, then end again.
dir=$(agent collab-edit start --session ed "$repo/docs/guide.md" | sed -n 's/^session //p')
agent edit "$dir" guide.md 's/Uno/Eins/'
sed -i 's/Uno/Un/' "$repo/docs/guide.md"
check "end: a conflict with the original stops the end" "stopped" \
  "$(agent collab-edit end "$dir" -m x >/dev/null 2>&1 && echo ended || echo stopped)"
check "end: the conflict is in the session's copy" "1" "$(grep -c '^<<<<<<<' "$dir/guide.md")"
sed -i -e '/^<<<<<<<\|^=======\|^>>>>>>>/d' -e '/^Un\.$/d' "$dir/guide.md"
git -C "$dir" commit -q -am "resolve"
agent collab-edit end "$dir" -m "guide: German one" >/dev/null
check "end: after the resolution, the end goes through" \
  "$(printf '# Guide\n\nEins.\nx\nDos.\nx\nTres.')" "$(cat "$repo/docs/guide.md")"

# --- comment-only mode (G5) ----------------------------------------------

dir=$(agent collab-edit start --comment-only --session ed "$repo/docs/guide.md" | sed -n 's/^session //p')
sed -i 's/^Dos\.$/<!-- agent: a better word? -->\n\nDos./' "$dir/guide.md"
check "comment-only: the agent may add a comment" "committed" \
  "$(agent git -C "$dir" commit -q -am c 2>/dev/null && echo committed || echo refused)"
sed -i 's/Dos/Two/' "$dir/guide.md"
check "comment-only: the agent may not change prose" "refused" \
  "$(agent git -C "$dir" commit -q -am p 2>/dev/null && echo committed || echo refused)"
check "comment-only: the user may change prose" "committed" \
  "$(user git -C "$dir" commit -q -am p 2>/dev/null && echo committed || echo refused)"
printf 'x\n' >"$dir/extra.md"
git -C "$dir" add extra.md
check "comment-only: the agent may not add a file" "refused" \
  "$(agent git -C "$dir" commit -q -m f 2>/dev/null && echo committed || echo refused)"
git -C "$dir" rm -q --cached extra.md
rm "$dir/extra.md"
sed -i '/^<!-- agent:/d' "$dir/guide.md"
agent git -C "$dir" commit -q -am "resolved"
agent collab-edit end "$dir" -m "guide: two" >/dev/null
check "comment-only: the end writes the user's change" "1" "$(grep -cx 'Two.' "$repo/docs/guide.md")"

if [ "$fails" -gt 0 ]; then
  echo "$fails failed"
  exit 1
fi
echo "all passed"
