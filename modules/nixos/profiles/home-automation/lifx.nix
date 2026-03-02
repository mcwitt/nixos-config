{ config, lib, ... }:
{
  config = lib.mkIf config.profiles.home-automation.enable {
    services.home-assistant.extraPackages =
      ps: with ps; [
        aiolifx
        aiolifx-effects
        aiolifx-themes
      ];
  };
}
