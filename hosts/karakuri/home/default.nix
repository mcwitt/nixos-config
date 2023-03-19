{ config, lib, ... }:

let
  dpi = 200;
in
{
  home.pointerCursor = {
    size = 64;
    x11.enable = true;
  };

  home.stateVersion = "21.11";

  programs.rofi.extraConfig.dpi = dpi;

  services.blueman-applet.enable = true;

  xresources.properties = {
    "*dpi" = dpi;
    "Xft.dpi" = dpi;
  };
}
