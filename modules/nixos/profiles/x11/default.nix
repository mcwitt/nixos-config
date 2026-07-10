{ config, lib, ... }:
{
  options.profiles.x11.enable = lib.mkEnableOption "X11 session stack (xserver, lightdm; xmonad on the home-manager side)";

  config = lib.mkIf config.profiles.x11.enable {
    # required for `gtk.enable = true` in home-manager
    programs.dconf.enable = true;

    services.xserver = {
      enable = true;

      desktopManager.xterm.enable = true;

      displayManager.lightdm = {
        enable = true;
        background = config.stylix.image;
        greeters.gtk.enable = true;
      };
    };
  };
}
