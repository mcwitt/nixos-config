{
  config,
  lib,
  pkgs,
  ...
}:
{
  config = lib.mkIf config.profiles.base.enable {
    programs.emacs = {
      overrides = self: _: {
        hunk-notes = self.trivialBuild {
          pname = "hunk-notes";
          version = "0.1.0-unstable-2026-06-13";
          src = pkgs.fetchFromGitHub {
            owner = "ArthurHeymans";
            repo = "emacs-hunk-notes";
            rev = "ba37f3d70927c028e5c15a0982cacac99279434d";
            hash = "sha256-sg2ZAiwB71Zi11P/uuzEdI4yXKiYmRLookHl7MX/Nbc=";
          };
          packageRequires = [ self.magit ];
        };
      };

      init.usePackage.hunk-notes = {
        enable = true;
        demand = true;
        config = ''
          (require 'hunk-notes-git)
          (require 'hunk-notes-magit)

          (setopt hunk-notes-auto-enable-in-diff-buffers t
                  hunk-notes-auto-enable-show-comments nil
                  hunk-notes-auto-enable-render-style 'overlay
                  hunk-notes-show-comments-on-enable t
                  hunk-notes-prompt-diff-scope 'commented-hunks
                  hunk-notes-agent-backends '(agent-shell copy buffer)
                  hunk-notes-default-agent 'agent-shell
                  hunk-notes-auto-submit-to-agent nil)
        '';
      };
    };
  };
}
