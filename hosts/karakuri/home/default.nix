{ config, lib, ... }:

let
  dpi = 200;
in
{
  home.stateVersion = "21.11";

  programs.rofi.extraConfig.dpi = dpi;

  services.blueman-applet.enable = true;

  services.polybar.settings = let colors = config.lib.stylix.colors.withHashtag; in {
    "bar/main" = {
      dpi = lib.mkForce dpi; # workaround; auto-detection fails
      modules-right = "wireless-network filesystem memory battery pulseaudio cpu";
    };
  };

  xresources.properties = {
    "*dpi" = dpi;
    "Xft.dpi" = dpi;
  };
}
