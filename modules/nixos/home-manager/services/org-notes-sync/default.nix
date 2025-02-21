{
  config,
  lib,
  pkgs,
  ...
}:
with lib;
let
  cfg = config.services.org-notes-sync;
in
{
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
      '';
    };
  };

  config = mkIf cfg.enable {
    systemd.user.services.org-notes-sync = {
      Unit = {
        Description = "Sync org-notes directory with remotes";
      };
      Service = {
        Environment =
          let
            paths = lib.makeBinPath (
              with pkgs;
              [
                findutils
                git
                openssh
              ]
            );
          in
          [ "PATH=${paths}" ];
        ExecStart = toString (
          let
            git-annex = pkgs.gitAndTools.git-annex;
          in
          pkgs.writeShellScript "org-notes-sync" ''
            set -eo pipefail
            export AWS_ACCESS_KEY_ID=$(${pkgs.awscli2}/bin/aws configure get default.aws_access_key_id)
            export AWS_SECRET_ACCESS_KEY=$(${pkgs.awscli2}/bin/aws configure get default.aws_secret_access_key)
            cd ${cfg.repoPath}
            ${git-annex}/bin/git-annex add .
            ${git-annex}/bin/git-annex sync --content
          ''
        );
      };
    };

    systemd.user.timers.org-notes-sync = {
      Unit = {
        Description = "Periodically sync org-notes directory";
      };
      Timer = {
        Unit = "org-notes-sync.service";
        OnCalendar = cfg.frequency;
        Persistent = true;
      };
      Install = {
        WantedBy = [ "timers.target" ];
      };
    };
  };
}
