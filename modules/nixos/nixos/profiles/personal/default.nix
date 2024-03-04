{ config, lib, ... }:
{
  config = lib.mkIf config.profiles.personal.enable {
    programs.steam = {
      enable = true;
      dedicatedServer.openFirewall = true;
      remotePlay.openFirewall = true;
    };
  };
}
