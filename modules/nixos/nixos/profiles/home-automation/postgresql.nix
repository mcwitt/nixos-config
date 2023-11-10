# https://nixos.wiki/wiki/Home_Assistant#Using_PostgreSQL
{ config, lib, pkgs, ... }:
{
  config = lib.mkIf config.profiles.home-automation.enable {
    services.home-assistant.config.recorder.db_url = "postgresql://@/hass";

    services.postgresql = {
      enable = true;

      # https://github.com/NixOS/nixpkgs/issues/216989
      package = pkgs.postgresql_14;

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
  };
}
