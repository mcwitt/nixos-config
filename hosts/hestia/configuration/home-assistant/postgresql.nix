# https://nixos.wiki/wiki/Home_Assistant#Using_PostgreSQL
{ pkgs, ... }:
{
  services.home-assistant.config.recorder.db_url = "postgresql://@/hass";

  services.postgresql = {
    enable = true;
    ensureDatabases = [ "hass" ];
    ensureUsers = [{
      name = "hass";
      ensurePermissions = {
        "DATABASE hass" = "ALL PRIVILEGES";
      };
    }];
  };

  services.postgresqlBackup = {
    enable = true;
    databases = [ "hass" ];
    startAt = "*-*-* 01:15:00";
  };
}
