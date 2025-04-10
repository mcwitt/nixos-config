{
  lib,
  pkgs,
  ...
}:

let
  dpi = 224;
in
{
  home.stateVersion = "21.11";

  programs.rofi.extraConfig.dpi = dpi;

  programs.spotify.package = pkgs.spotify.override { deviceScaleFactor = 2.0; };

  services.blueman-applet.enable = true;

  services.polybar.settings = {
    "bar/main" = {
      dpi = lib.mkForce dpi; # workaround; auto-detection fails
      modules-right = "wireless-network filesystem memory battery cpu pipewire";
    };
  };

  xresources.properties."Xft.dpi" = dpi;
}
