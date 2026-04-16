{
  config,
  lib,
  pkgs,
  ...
}:
# https://nixos.wiki/wiki/PipeWire
# rtkit is optional but recommended
{
  config = lib.mkIf config.profiles.desktop.enable {
    environment.systemPackages = [ pkgs.alsa-utils ];

    security.rtkit.enable = true;

    services.pipewire = {
      enable = true;
      alsa.enable = true;
      alsa.support32Bit = true;
      pulse.enable = true;
    };
  };
}
