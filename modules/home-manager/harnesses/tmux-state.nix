# `agent-tmux-state <state>`: publish an agent session's state as the tmux
# pane option @agent_state on $TMUX_PANE, so the tmux status line can count
# agents by state and the jump binding can find the ones that need attention.
# The tmux side (status-right, window-status-format, the jump binding) lives
# in modules/home-manager/profiles/base/tmux.nix.
#
# Each harness runs this from its own hooks, plugin or extension: Claude Code
# (claude-code/tmux-state.nix), Codex (codex/tmux-state.nix), OpenCode
# (opencode/tmux-state.ts) and pi (pi/tmux-state.ts).
#
# States:
#   working     the agent is busy.
#   input       blocked on a permission prompt or a question; rings the bell.
#   done        idle at the prompt.
#   background  the turn ended but background tasks are still running.
#   off         the session ended; unsets the option.
{ config, pkgs }:
let
  tmux = "${config.programs.tmux.package}/bin/tmux";
in
pkgs.writeShellScript "agent-tmux-state" ''
  # Hooks pass a JSON payload on stdin. Read it even though it is unused, so
  # the harness never sees a closed pipe.
  [[ -t 0 ]] || cat >/dev/null
  [[ -n "''${TMUX_PANE:-}" ]] || exit 0

  state=$1
  case "$state" in
    off) ${tmux} set -pu -t "$TMUX_PANE" @agent_state ;;
    *) ${tmux} set -p -t "$TMUX_PANE" @agent_state "$state" ;;
  esac

  # Ring the pane's bell for a blocking request: tmux then raises its own
  # bell flag on the window and passes the bell to an attached terminal.
  if [[ "$state" == input ]]; then
    tty=$(${tmux} display -p -t "$TMUX_PANE" '#{pane_tty}')
    [[ -w "$tty" ]] && printf '\a' >"$tty"
  fi
  exit 0
''
