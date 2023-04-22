{ lib, pkgs, ... }:

let
  dpi = 183;
in
{
  home.stateVersion = "21.11";

  # TODO: clean up when https://github.com/NixOS/nixpkgs/issues/227449 fixed
  programs.spotify.package = pkgs.spotify.override {
    callPackage = p: attrs: pkgs.callPackage p (attrs // { deviceScaleFactor = 1.8; });
  };

  programs.rofi.extraConfig.dpi = dpi;

  xresources.properties = {
    "*dpi" = dpi;
    "Xft.dpi" = dpi;
  };
}
