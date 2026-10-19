# Publish each Claude Code session's state as the tmux window option
# @agent_state, so the tmux status line can show which agents need input.
# The tmux side (status-right, window-status-format, the jump binding) lives
# in modules/home-manager/profiles/base/tmux.nix.
{ config, pkgs }:
let
  tmux = "${config.programs.tmux.package}/bin/tmux";

  script = pkgs.writeShellScript "claude-tmux-state" ''
    # Hooks receive a JSON payload on stdin; drain it so Claude Code never
    # blocks on a full pipe.
    cat >/dev/null
    [[ -n "''${TMUX_PANE:-}" ]] || exit 0

    case "$1" in
      off) ${tmux} set -wu -t "$TMUX_PANE" @agent_state ;;
      *) ${tmux} set -w -t "$TMUX_PANE" @agent_state "$1" ;;
    esac

    # Ring the pane's bell for a blocking request: tmux then raises its own
    # bell flag on the window and passes the bell to an attached terminal.
    if [[ "$1" == input ]]; then
      tty=$(${tmux} display -p -t "$TMUX_PANE" '#{pane_tty}')
      [[ -w "$tty" ]] && printf '\a' >"$tty"
    fi
    exit 0
  '';

  hook = state: [
    {
      type = "command";
      command = "${script} ${state}";
      timeout = 5;
    }
  ];
  on = matcher: state: {
    inherit matcher;
    hooks = hook state;
  };
in
{
  # working: Claude is busy. input: blocked on a permission prompt or a
  # question. done: the turn ended and Claude is idle at the prompt.

  # Not on "compact", which can happen mid-turn.
  SessionStart = [ (on "^(startup|resume|clear)$" "done") ];
  UserPromptSubmit = [ (on "" "working") ];
  PreToolUse = [ (on "^(AskUserQuestion|ExitPlanMode)$" "input") ];
  PermissionRequest = [ (on "" "input") ];
  # Covers the permission or question being answered.
  PostToolUse = [ (on "" "working") ];
  Notification = [
    (on "^(permission_prompt|elicitation_dialog)$" "input")
    (on "^idle_prompt$" "done")
  ];
  Stop = [ (on "" "done") ];
  SessionEnd = [ (on "" "off") ];
}
