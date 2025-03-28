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
      anki
      discord
      element-desktop
      gimp
      inkscape
      slack
      zoom-us
    ];

    programs.spotify.enable = true;
  };
}
