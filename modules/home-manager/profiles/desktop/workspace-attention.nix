{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.profiles.desktop;
  workspace-attention = pkgs.writeShellApplication {
    name = "workspace-attention";
    runtimeInputs = [
      pkgs.coreutils # sha1sum, realpath, mktemp, mv, cut, mkdir
      pkgs.xmonadctl # the "agent-attention-refresh" poke
    ];
    text = builtins.readFile ./xmonad/workspace-attention.sh;
  };
in
{
  options.profiles.desktop.workspaceAttention.package = lib.mkOption {
    type = lib.types.package;
    readOnly = true;
    default = workspace-attention;
    description = "CLI that flags a workspace as needing attention (consumed by xmonad.hs). Referenced by the Claude Code Stop hook.";
  };

  config = lib.mkIf cfg.enable {
    home.packages = [
      workspace-attention
      pkgs.xmonadctl
    ];
  };
}
