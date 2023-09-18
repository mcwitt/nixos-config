{ config, lib, pkgs, ... }:

let
  cfg = config.services.duckdns;
in
{
  options.services.duckdns = {
    enable = lib.mkEnableOption "Whether to enable the DuckDNS updater service.";
    domain = lib.mkOption { type = lib.types.str; };
    tokenPath = lib.mkOption {
      type = lib.types.path;
      description = "Path to file containing the authentication token.";
    };
  };

  config = lib.mkIf cfg.enable {
    systemd = {
      services.duckdns = {
        description = "DuckDNS updater service";
        script = ''
          token=$(cat ${cfg.tokenPath})
          ${pkgs.curl}/bin/curl "https://www.duckdns.org/update?domains=${cfg.domain}&token=$token"
        '';
        serviceConfig.User = "duckdns";
        startAt = "hourly";
      };

      timers.duckdns.timerConfig.RandomizedDelaySec = "15m";
    };

    users = {
      users.duckdns = {
        isSystemUser = true;
        group = "duckdns";
      };
      groups.duckdns = { };
    };
  };
}
