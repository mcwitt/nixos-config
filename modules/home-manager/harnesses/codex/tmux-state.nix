# Codex hooks that publish each session's state as the tmux pane option
# @agent_state; see ../tmux-state.nix for the states and the script. The
# hooks find the pane through $TMUX_PANE, which is why ./default.nix keeps
# sessions off the shared background server.
{
  config,
  lib,
  pkgs,
}:
let
  script = import ../tmux-state.nix { inherit config pkgs; };

  # PermissionRequest fires before Codex routes the approval. With
  # approvals_reviewer = "auto_review" the reviewer agent decides and nobody
  # is asked, so only report input when approvals go to the user, as read
  # from the user config when the hook runs.
  permissionState = pkgs.writeShellScript "codex-tmux-permission" ''
    cat >/dev/null
    reviewer=$(${pkgs.gawk}/bin/awk -F '"' '/^\[/ { exit } /^approvals_reviewer *=/ { print $2; exit }' \
      "''${CODEX_HOME:-$HOME/.codex}/config.toml" 2>/dev/null)
    case "$reviewer" in
      auto_review | guardian_subagent) exit 0 ;;
    esac
    exec ${script} input
  '';

  tmuxHooks = {
    # Not on "compact", which can happen mid-turn.
    SessionStart = [
      {
        matcher = "startup|resume|clear|fork";
        command = "${script} done";
      }
    ];
    UserPromptSubmit = [ { command = "${script} working"; } ];
    # Plan mode's question tool.
    PreToolUse = [
      {
        matcher = "request_user_input";
        command = "${script} input";
      }
    ];
    PermissionRequest = [ { command = "${permissionState}"; } ];
    # Covers the permission or question being answered.
    PostToolUse = [ { command = "${script} working"; } ];
    Stop = [ { command = "${script} done"; } ];
    Interrupt = [ { command = "${script} done"; } ];
  };
  hookGroup =
    event:
    {
      matcher ? null,
      command,
    }:
    lib.optionalAttrs (matcher != null) { inherit matcher; }
    // {
      hooks = [
        {
          type = "command";
          inherit command;
          # Interrupt hooks are capped at 3 seconds.
          timeout = if event == "Interrupt" then 3 else 5;
        }
      ];
    };
in
{
  inherit script;
  hooks = lib.mapAttrs (event: map (hookGroup event)) tmuxHooks;
}
