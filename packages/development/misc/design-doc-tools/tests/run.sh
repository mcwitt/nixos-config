#!/usr/bin/env bash
# Tests for design-doc-wc.
set -euo pipefail

here=$(cd "$(dirname "$0")" && pwd)
tools=$(dirname "$here")
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

if [ "$fails" -gt 0 ]; then
  echo "$fails failed"
  exit 1
fi
echo "all passed"
