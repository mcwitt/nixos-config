{ config, lib, ... }:
{
  imports = [ ./pipewire.nix ];

  options.profiles.desktop.enable = lib.mkEnableOption "Profile for use on machines that run a graphical desktop";

  config = lib.mkIf config.profiles.desktop.enable {
    # required for `gtk.enable = true` in home-manager
    programs.dconf.enable = true;
  };
}
