{ config, lib, ... }:
{
  config = lib.mkIf config.profiles.base.enable {

    programs.emacs.init.usePackage = {

      frames-only-mode.config = lib.mkBefore ''
        (add-to-list 'frames-only-mode-use-window-functions #'org-capture)
      '';

      org = {
        enable = true;
        hook = [
          "(org-mode . turn-on-visual-line-mode)"
          "(org-mode . turn-on-flyspell)"
        ];
        bind = {
          # Standard global bindings
          # [[info:org#Activation][org#Activation]]
          "C-c l" = "org-store-link";
          "C-c a" = "org-agenda";
          "C-c c" = "org-capture";
        };
        custom = {
          org-directory = ''"~/org"'';
          org-startup-indented = true;
          org-agenda-files = ''
            '("~/org/gtd.org"
              "~/org/inbox.org")
          '';
          org-agenda-block-separator = "?─";
          org-agenda-custom-commands = ''
            '(("n" "Agenda and NEXT TODOs"
               ((agenda "")
                (todo "NEXT")
                (tags-todo "CATEGORY=\"Inbox\"" ((org-agenda-overriding-header "Inbox")))
                (stuck ""))))
          '';
          org-agenda-prefix-format = ''
            '((agenda . " %i %-12:c%?-12t% s")
              (todo . " %i %-12:c%b")
              (tags . " %i %-12:c")
              (search . " %i %-12:c"))
          '';
          org-agenda-breadcrumbs-separator = ''"/"'';
          org-capture-templates = ''
            '(("t" "todo" entry (file "~/org/inbox.org") "* TODO %?")
              ("l" "link" entry (file "~/org/inbox.org") "* TODO %(org-cliplink-capture)" :immediate-finish t))
          '';
          org-refile-targets = '''(("~/org/gtd.org" :maxlevel . 10))'';
          org-stuck-projects = '''("+LEVEL=2+PROJECT/-DONE" ("NEXT") nil "")'';
        };
      };

      org-cliplink = {
        enable = true;
        command = [ "org-cliplink-capture" ];
      };

      org-roam = {
        enable = true;
        init = ''
          (make-directory "~/org/roam" t)
        '';
        bind = {
          "C-c n c" = "org-roam-capture";
          "C-c n f" = "org-roam-node-find";
          "C-c n i" = "org-roam-node-insert";
        };
        custom = {
          org-roam-directory = ''"~/org/roam"'';
          org-roam-dailies-directory = ''"daily/"'';
          org-roam-dailies-capture-templates = ''
            '(("d" "default" entry
               "* %?"
               :target (file+head "%<%Y-%m-%d>.org"
                                  "#+title: %<%Y-%m-%d>\n")))
          '';
        };
        config = ''
          (org-roam-db-autosync-mode)
        '';
      };
    };
  };
}
