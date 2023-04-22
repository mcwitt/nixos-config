{ config, lib, pkgs, ... }:

let
  dpi = 200;
in
{
  home.stateVersion = "21.11";

  programs.rofi.extraConfig.dpi = dpi;

  # TODO: clean up when https://github.com/NixOS/nixpkgs/issues/227449 fixed
  programs.spotify.package = pkgs.spotify.override {
    callPackage = p: attrs: pkgs.callPackage p (attrs // { deviceScaleFactor = 2.0; });
  };

  services.blueman-applet.enable = true;

  services.polybar.settings = let colors = config.lib.stylix.colors.withHashtag; in {
    "bar/main" = {
      dpi = lib.mkForce dpi; # workaround; auto-detection fails
      modules-right = "wireless-network filesystem memory battery cpu pulseaudio";
    };
  };

  xresources.properties = {
    "*dpi" = dpi;
    "Xft.dpi" = dpi;
  };
}
