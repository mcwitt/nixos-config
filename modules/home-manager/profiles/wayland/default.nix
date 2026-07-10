{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.profiles.wayland;
in
{
  options.profiles.wayland.enable = lib.mkEnableOption "Profile for the ewm (Wayland) graphical desktop";

  # Session-specific pieces only; shared applications live in profiles.gui-apps.
  imports = [
    ./emacs-ewm.nix
    ./services.nix
    ./waybar.nix
  ];

  config = lib.mkIf (cfg.enable && pkgs.stdenv.isLinux) {
    stylix.enable = true;
  };
}
