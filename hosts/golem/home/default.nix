{ lib, ... }:

let
  dpi = 183;
in
{
  home.stateVersion = "21.11";

  nixpkgs.overlays =
    let
      overlay = _: prev: {
        spotify = prev.spotify.override { deviceScaleFactor = 2.0; };
      };
    in
    [ overlay ];

  programs.rofi.extraConfig.dpi = dpi;

  xresources.properties = {
    "*dpi" = dpi;
    "Xft.dpi" = dpi;
  };
}
