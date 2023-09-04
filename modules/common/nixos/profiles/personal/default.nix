{ config, lib, ... }:
{
  options.profiles.personal.enable = lib.mkEnableOption "Profile for use on machines I own";

  config = lib.mkIf config.profiles.personal.enable {
    services.geoclue2 = {
      enable = true;
      appConfig = {
        "gammastep" = {
          isAllowed = true;
          isSystem = false;
        };
      };
    };
  };
}
