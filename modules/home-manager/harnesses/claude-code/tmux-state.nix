# Claude Code hooks that publish each session's state as the tmux window
# option @agent_state; see ../tmux-state.nix for the states and the script.
{ config, pkgs }:
let
  tmux = "${config.programs.tmux.package}/bin/tmux";
  shared = import ../tmux-state.nix { inherit config pkgs; };

  # The shared states, plus two for Claude Code's hooks:
  #   stop  the turn ended: background if the payload on stdin lists running
  #         background subagents or shell commands, which will wake Claude
  #         when they finish, else done.
  #   idle  Claude has sat at the prompt for a while: done, but only if the
  #         state is still working. This corrects a session interrupted
  #         without a hook, which would otherwise stay working.
  script = pkgs.writeShellScript "claude-tmux-state" ''
    payload=
    [[ -t 0 ]] || payload=$(cat)
    [[ -n "''${TMUX_PANE:-}" ]] || exit 0

    state=$1
    case "$state" in
      stop)
        if ${pkgs.jq}/bin/jq -e 'any(.background_tasks[]?; .status == "running")' \
          <<<"$payload" >/dev/null 2>&1; then
          state=background
        else
          state=done
        fi
        ;;
      idle)
        [[ $(${tmux} show -wv -t "$TMUX_PANE" @agent_state 2>/dev/null) == working ]] || exit 0
        state=done
        ;;
    esac
    exec ${shared} "$state" </dev/null
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
