{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.profiles.desktop;
  claude-agents-attach = pkgs.writeShellApplication {
    name = "claude-agents-attach";
    # Only jq is pinned; rofi, wezterm, and claude are resolved from the session
    # PATH (writeShellApplication prepends runtimeInputs, keeping inherited PATH),
    # matching how xmonad already spawns bare `rofi`/`wezterm`. This also avoids
    # pinning a rofi/wezterm that differs from the user's configured one.
    runtimeInputs = [ pkgs.jq ];
    text = builtins.readFile ./claude-agents-attach.sh;
  };
in
{
  config = lib.mkIf cfg.enable {
    home.packages = [ claude-agents-attach ];
  };
}
