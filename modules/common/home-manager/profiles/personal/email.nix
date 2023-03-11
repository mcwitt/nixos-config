{ config, lib, ... }:
let cfg = config.profiles.personal;
in
{
  config = lib.mkIf cfg.enable {

    accounts.email.maildirBasePath = ".mail";

    accounts.email.accounts."mcwitt@gmail.com" = {
      primary = true;
      address = "mcwitt@gmail.com";
      userName = "mcwitt";
      realName = "Matt Wittmann";
      flavor = "gmail.com"; # sets imap and smtp server parameters

      mu.enable = true;

      mbsync = {
        enable = true;

        create = "maildir";

        extraConfig.channel = {
          Patterns = [
            "*"
            "![Gmail]*"
            "[Gmail]/Sent Mail"
            "[Gmail]/Starred"
            "[Gmail]/All Mail"
          ];
        };
      };

      smtp.tls = {
        enable = true;
        useStartTls = true;
      };
    };

    programs.emacs.init.usePackage = {
      mu4e.enable = true;

      mu4e-column-faces = {
        enable = true;
        after = [ "mu4e" ];
        config = ''
          (mu4e-column-faces-mode)
        '';
      };

      smtpmail = let gmail = config.accounts.email.accounts."mcwitt@gmail.com"; in {
        enable = true;
        config = ''
          (setq send-mail-function 'smtpmail-send-it)
          (setq smtpmail-smtp-server "${gmail.smtp.host}")
          (setq smtpmail-smtp-service ${toString gmail.smtp.port})
          (setq smtpmail-smtp-user "${gmail.address}")
        '';
      };
    };

    programs.mu.enable = true;

    programs.mbsync.enable = true;

    services.mbsync.enable = true;
  };
}
