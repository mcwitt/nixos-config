{ config, lib, ... }:
{
  home.pointerCursor = {
    size = 64;
    x11.enable = true;
  };

  home.stateVersion = "21.11";

  programs.rofi.font = lib.mkForce "Iosevka Comfy 24"; # HACK since dpi setting in Xresources seems ignored

  services.blueman-applet.enable = true;

  xresources.properties = {
    "*dpi" = 200;
    "Xft.dpi" = 200;
  };
}
