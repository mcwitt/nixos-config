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
      # peek # broken 2023-08-07
      slack
      zoom-us
      zulip
    ];

    programs.spotify.enable = true;
  };
}
