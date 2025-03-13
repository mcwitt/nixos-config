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
          "(org-capture-mode-hook . evil-insert-state)"
        ];

        bind = {
          "C-c l" = "org-store-link";
          "C-c a" = "org-agenda";
          "C-c c" = "org-capture";
        };

        bindLocal.org-agenda-mode-map = {
          j = "evil-next-line";
          k = "evil-previous-line";
        };

        custom = {
          org-directory = ''"~/org"'';
          org-startup-indented = true;
          org-agenda-files = ''
            '("~/org/gtd.org"
              "~/org/inbox.org")
          '';
          org-agenda-block-separator = "nil";
          org-agenda-custom-commands = ''
            '(("n" "Agenda and NEXT TODOs"
               ((agenda "")
                (todo "NEXT" ((org-agenda-overriding-header "In Progress:")))
                (tags-todo "CATEGORY=\"Inbox\"" ((org-agenda-overriding-header "Inbox:")))
                (stuck ""))))
          '';
          org-agenda-prefix-format = ''
            '((agenda . " %i %-12:c%?-12t% s")
              (todo . " %i %-12:c%b")
              (tags . " %i %-12:c")
              (search . " %i %-12:c"))
          '';
          org-agenda-breadcrumbs-separator = ''"/"'';
          org-capture-templates = '''(("t" "todo" entry (file "~/org/inbox.org") "* TODO %a%?\n%i"))'';
          org-refile-targets = '''(("~/org/gtd.org" :maxlevel . 10))'';
          org-stuck-projects = '''("+LEVEL=2+PROJECT/-DONE" ("NEXT") nil "")'';
        };

        config = ''
          ;; The following is used to treat frames named "org-capture"
          ;; as dedicated capture frames, meaning they will
          ;;
          ;; 1. only display the capture buffer, and
          ;; 2. be deleted when the capture is finalized.
          ;;
          ;; This is useful for launching a dedicated capture frame with a command like
          ;;
          ;;     emacsclient -c -n -F '((name . "org-capture-dedicated"))' -e '(org-capture nil "t")'
          ;;
          ;; TODO: Is there a cleaner way to accomplish this?

          (defun my/dedicated-org-capture-frame-p ()
            "Return whether the current frame is a dedicated capture frame."
            (equal (frame-parameter nil 'name) "org-capture-dedicated"))

          (defun my/dedicated-org-capture-frame-delete-other-windows ()
            "Delete other windows if the current frame is a dedicated capture frame."
            (when (my/dedicated-org-capture-frame-p) (delete-other-windows)))

          (add-hook 'org-capture-mode-hook #'my/dedicated-org-capture-frame-delete-other-windows)

          (defun my/dedicated-org-capture-frame-cleanup ()
            "Delete the frame if it is a dedicated capture frame."
            (when (my/dedicated-org-capture-frame-p) (delete-frame)))

          (add-hook 'org-capture-after-finalize-hook #'my/dedicated-org-capture-frame-cleanup)
        '';
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

    # https://orgmode.org/worg/org-contrib/org-protocol.html
    xdg.desktopEntries.org-protocol = {
      name = "org-protocol";
      comment = "Intercept calls from emacsclient to trigger custom actions";
      categories = [ "X-Other" ];
      icon = "emacs";
      type = "Application";
      exec = "emacsclient -- %u";
      terminal = false;
      mimeType = [ "x-scheme-handler/org-protocol" ];
    };
  };
}
