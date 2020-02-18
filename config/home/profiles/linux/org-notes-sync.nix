{ config, lib, pkgs, ... }:
with lib;
let cfg = config.services.org-notes-sync;
in {
  options.services.org-notes-sync = {
    enable = mkEnableOption "org notes sync";
    repoPath = mkOption {
      type = types.str;
      description = "Path of the local repository";
    };
    frequency = mkOption {
      type = types.str;
      default = "*:0/5";
      description = ''
        How often to synchronize the org-notes repository with its default upstream.
        </para><para>
        This value is passed to the systemd timer configuration as the
        <literal>onCalendar</literal> option.
        See
        <citerefentry>
          <refentrytitle>systemd.time</refentrytitle>
          <manvolnum>7</manvolnum>
        </citerefentry>
        for more information about the format.
      '';
    };
  };

  config = mkIf cfg.enable {
    systemd.user.services.org-notes-sync = {
      Unit = { Description = "Org notes sync"; };

      Service = {
        CPUSchedulingPolicy = "idle";
        IOSchedulingClass = "idle";
        ExecStart = toString (pkgs.writeShellScript "org-notes-sync" ''
          cd ${cfg.repoPath} \
          && ${pkgs.gitAndTools.git-sync}/bin/git-sync
        '');
      };
    };

    systemd.user.timers.org-notes-sync = {
      Unit = { Description = "Org notes periodic sync"; };

      Timer = {
        Unit = "org-notes-sync.service";
        OnCalendar = cfg.frequency;
        Persistent = true;
      };

      Install = { WantedBy = [ "timers.target" ]; };
    };
  };
}
