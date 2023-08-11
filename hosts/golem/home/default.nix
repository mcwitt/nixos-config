{ pkgs, ... }:

let
  dpi = 183;
in
{
  imports = [ ./feh.nix ];

  home.stateVersion = "21.11";

  programs.spotify.package = pkgs.spotify.override { deviceScaleFactor = 1.8; };

  programs.rofi.extraConfig.dpi = dpi;

  services.polybar.script = ''
    MONITOR=DP-4 polybar &
    MONITOR=DP-2 polybar &
  '';

  xresources.properties = {
    "*dpi" = dpi;
    "Xft.dpi" = dpi;
  };
}

