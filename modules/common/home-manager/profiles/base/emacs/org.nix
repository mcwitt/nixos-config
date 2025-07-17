{
  config,
  lib,
  ...
}:
{
  config = lib.mkIf config.profiles.base.enable {

    programs.emacs.init.usePackage = {

      anki-editor.enable = true;

      cdlatex.enable = true;

      frames-only-mode.config = ''
        (add-to-list 'frames-only-mode-use-window-functions #'org-capture)
        (add-to-list 'frames-only-mode-use-window-functions #'org-roam-buffer-toggle)
      '';

      mixed-pitch = {
        enable = true;
        hook = [ "(org-mode . mixed-pitch-mode)" ];
        diminish = [ "mixed-pitch-mode" ];
      };

      org = {
        enable = true;

        hook = [
          "(org-mode . turn-on-flyspell)"
          "(org-mode . turn-on-visual-line-mode)"
          "(org-mode . org-appear-mode)"
          "(org-capture-mode . evil-insert-state)"
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

          org-hide-emphasis-markers = true;
          org-hidden-keywords = "'(title subtitle author date email)";
          org-pretty-entities = true;

          org-highlight-latex-and-related = '''(native)'';

          org-agenda-files = ''
            '("~/org/gtd.org"
              "~/org/inbox.org"
              "~/org/tickler.org")
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

          org-capture-templates = ''
            '(("t" "todo" entry (file "~/org/inbox.org") "* TODO %a%?\n%i")
              ("k" "tickler" entry (file "~/org/tickler.org") "* %i%?"))
          '';
          org-refile-targets = '''(("~/org/gtd.org" :maxlevel . 3))'';
          org-stuck-projects = '''("+LEVEL=2+PROJECT/-DONE" ("NEXT") nil "")'';
        };

        config = ''
          ;; Resize Org headings
          ;; https://sophiebos.io/posts/prettifying-emacs-org-mode/
          (dolist (face '((org-level-1 . 1.35)
                          (org-level-2 . 1.3)
                          (org-level-3 . 1.2)
                          (org-level-4 . 1.1)
                          (org-level-5 . 1.1)
                          (org-level-6 . 1.1)
                          (org-level-7 . 1.1)
                          (org-level-8 . 1.1)))
            (set-face-attribute (car face) nil :weight 'bold :height (cdr face)))

          ;; Make the document title a bit bigger
          (set-face-attribute 'org-document-title nil :weight 'bold :height 1.8)

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

      org-appear = {
        enable = true;
        custom = {
          org-appear-autoemphasis = true;
          org-appear-autolinks = true;
          org-appear-autokeywords = true;
        };
      };

      org-modern = {
        enable = true;
        after = [ "org" ];
        config = ''
          (global-org-modern-mode)
        '';
      };

      org-protocol.enable = true;

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
          ;; Author-recommended buffer display settings
          ;; https://www.orgroam.com/manual.html#Configuring-the-Org_002droam-buffer-display-1
          (add-to-list 'display-buffer-alist
                       '("\\*org-roam\\*"
                         (display-buffer-in-direction)
                         (direction . right)
                         (window-width . 0.33)
                         (window-height . fit-window-to-buffer)))

          (org-roam-db-autosync-mode)
        '';
      };

      ox-pandoc.enable = true;
    };
  };
}
