{ config, lib, ... }:
{
  config = lib.mkIf config.profiles.home-automation.enable {

    services.home-assistant = {
      config.recorder = {
        db_url = "postgresql://@/hass"; # https://nixos.wiki/wiki/Home_Assistant#Using_PostgreSQL
        auto_purge = true;
        purge_keep_days = 365;
        commit_interval = 5;
      };
      extraPackages = ps: [ ps.psycopg2 ];
    };

    services.postgresql = {
      enable = true;

      ensureDatabases = [ "hass" ];

      ensureUsers = [{
        name = "hass";
        ensureDBOwnership = true;
      }];
    };

    services.postgresqlBackup = {
      enable = true;
      databases = [ "hass" ];
      startAt = "*-*-* 01:15:00";
    };
  };
}
