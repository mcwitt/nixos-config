{ lib, ... }:
{
  home.stateVersion = "21.11";

  nixpkgs.overlays =
    let
      overlay = _: prev: {
        spotify = prev.spotify.override { deviceScaleFactor = 2.0; };
      };
    in
    [ overlay ];

  programs.rofi.font = lib.mkForce "Iosevka Comfy 20"; # HACK since dpi setting in Xresources seems ignored

  xresources.properties = {
    "*dpi" = 183;
    "Xft.dpi" = 183;
  };
}
