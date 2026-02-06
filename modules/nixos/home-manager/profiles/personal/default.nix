{
  config,
  lib,
  pkgs,
  ...
}:
with lib;
{
  config = mkIf config.profiles.personal.enable {
    home.packages = with pkgs; [
      (anki.withAddons [ ankiAddons.anki-connect ])
      discord
      gimp
      inkscape
      signal-desktop
      slack
      zoom-us
    ];

    programs.ncspot.enable = true;
    programs.spotify.enable = true;
  };
}
