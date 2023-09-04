{ config, lib, ... }:
{
  config = lib.mkIf config.profiles.personal.enable {
    programs.steam.enable = true;
  };
}
