# Publish each Claude Code session's state as the tmux window option
# @agent_state, so the tmux status line can show which agents need input.
# The tmux side (status-right, window-status-format, the jump binding) lives
# in modules/home-manager/profiles/base/tmux.nix.
{ config, pkgs }:
let
  tmux = "${config.programs.tmux.package}/bin/tmux";

  script = pkgs.writeShellScript "claude-tmux-state" ''
    payload=$(cat)
    [[ -n "''${TMUX_PANE:-}" ]] || exit 0

    state=$1
    case "$state" in
      # The turn ended: idle, unless background subagents or shell commands
      # are still running and will wake Claude when they finish.
      stop)
        if ${pkgs.jq}/bin/jq -e 'any(.background_tasks[]?; .status == "running")' \
          <<<"$payload" >/dev/null; then
          state=background
        else
          state=done
        fi
        ;;
      # Claude has sat at the prompt for a minute. This only corrects a
      # session interrupted with Esc, which fires no hook and stays "working".
      idle)
        [[ $(${tmux} show -wv -t "$TMUX_PANE" @agent_state 2>/dev/null) == working ]] || exit 0
        state=done
        ;;
    esac

    case "$state" in
      off) ${tmux} set -wu -t "$TMUX_PANE" @agent_state ;;
      *) ${tmux} set -w -t "$TMUX_PANE" @agent_state "$state" ;;
    esac

    # Ring the pane's bell for a blocking request: tmux then raises its own
    # bell flag on the window and passes the bell to an attached terminal.
    if [[ "$state" == input ]]; then
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
  # working: Claude is busy. background: the turn ended but background
  # subagents or shell commands are still running. input: blocked on a
  # permission prompt or a question. done: idle at the prompt.

  # Not on "compact", which can happen mid-turn.
  SessionStart = [ (on "^(startup|resume|clear)$" "done") ];
  UserPromptSubmit = [ (on "" "working") ];
  PreToolUse = [ (on "^(AskUserQuestion|ExitPlanMode)$" "input") ];
  PermissionRequest = [ (on "" "input") ];
  # Covers the permission or question being answered.
  PostToolUse = [ (on "" "working") ];
  Notification = [
    (on "^(permission_prompt|elicitation_dialog)$" "input")
    (on "^idle_prompt$" "idle")
  ];
  Stop = [ (on "" "stop") ];
  SessionEnd = [ (on "" "off") ];
}
