{ config, lib, ... }:
{
  config = lib.mkIf config.profiles.home-automation.enable {
    services.home-assistant = {
      extraComponents = [
        "forecast_solar"
        "solaredge"
      ];

      extraPackages = ps: with ps; [
        aiosolaredge
      ];
    };
  };
}
