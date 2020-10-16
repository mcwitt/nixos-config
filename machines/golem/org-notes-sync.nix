{ config, lib, pkgs, ... }:
with lib;
let cfg = config.services.org-notes-sync;
in {
  options.services.org-notes-sync = {
    enable = mkEnableOption "org-notes sync";
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
      Unit = { Description = "Sync org-notes directory with remotes"; };

      Service = {
        CPUSchedulingPolicy = "idle";
        IOSchedulingClass = "idle";
        Environment = let
          paths = lib.makeBinPath
            (with pkgs; [ findutils git gitAndTools.git-annex openssh ]);
        in [ "PATH=${paths}" ];
        ExecStart = toString (pkgs.writeShellScript "org-notes-sync" ''
          set -eo pipefail
          export AWS_ACCESS_KEY_ID=$(${pkgs.awscli2}/bin/aws configure get default.aws_access_key_id)
          export AWS_SECRET_ACCESS_KEY=$(${pkgs.awscli2}/bin/aws configure get default.aws_secret_access_key)
          cd ${cfg.repoPath}
          ${pkgs.gitAndTools.git-annex}/bin/git-annex add .
          ${pkgs.gitAndTools.git-annex}/bin/git-annex sync --content
        '');
      };
    };

    systemd.user.timers.org-notes-sync = {
      Unit = { Description = "Periodically sync org-notes directory"; };

      Timer = {
        Unit = "org-notes-sync.service";
        OnCalendar = cfg.frequency;
        Persistent = true;
      };

      Install = { WantedBy = [ "timers.target" ]; };
    };
  };
}
