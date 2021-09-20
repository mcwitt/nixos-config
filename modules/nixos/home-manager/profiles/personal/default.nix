{ config, lib, pkgs, ... }:
with lib;
{
  config = mkIf config.profiles.personal.enable {
    home.packages = with pkgs; [
      anki
      discord
      element-desktop
      gimp
      peek
      slack
      zoom-us
      zulip
    ];
  };
}
