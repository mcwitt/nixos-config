{ lib, pkgs, ... }:

let
  dpi = 183;
in
{
  home.stateVersion = "21.11";

  programs.spotify.package = pkgs.spotify.override { deviceScaleFactor = 1.8; };

  programs.rofi.extraConfig.dpi = dpi;

  xresources.properties = {
    "*dpi" = dpi;
    "Xft.dpi" = dpi;
  };
}
