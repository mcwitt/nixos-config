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

      elfeed-org = {
        enable = true;
        after = [ "org" ];
        config = ''
          (setopt rmh-elfeed-org-files (list (expand-file-name "feeds.org" org-directory)))
          (elfeed-org)
        '';
      };

      htmlize.enable = true;

      org = {
        enable = true;

        hook = [
          "(org-mode . flyspell-mode)"
          "(org-mode . turn-on-visual-line-mode)"
          "(org-mode . variable-pitch-mode)"
          "(org-capture-mode . evil-insert-state)"
        ];

        bind = {
          "C-c l" = "org-store-link";
          "C-c a" = "org-agenda";
          "C-c c" = "org-capture";
        };

        config = ''
          (setopt org-directory "~/org"
                  org-hide-emphasis-markers t
                  org-hidden-keywords '(title subtitle author date email)
                  org-pretty-entities t

                  ;; don't right-justify tags (looks bad with variable-width font)
                  ;; instead, put tags immediately after the headline
                  org-auto-align-tags nil
                  org-tags-column 0

                  org-highlight-latex-and-related '(native)
                  org-preview-latex-default-process 'dvisvgm

                  org-agenda-files '("gtd.org" "inbox.org" "routines.org" "tickler.org" "anniversaries.org")
                  org-agenda-block-separator nil
                  org-agenda-custom-commands '(("n" "Agenda and NEXT TODOs"
                                                ((agenda "")
                                                 (todo "NEXT" ((org-agenda-overriding-header "In Progress:")))
                                                 (tags-todo "CATEGORY=\"Inbox\"" ((org-agenda-overriding-header "Inbox:")))
                                                 (stuck ""))))
                  org-agenda-prefix-format '((agenda . " %i %-12:c%?-12t% s")
                                             (todo . " %i %-12:c%b")
                                             (tags . " %i %-12:c")
                                             (search . " %i %-12:c"))
                  org-agenda-breadcrumbs-separator "/"

                  org-capture-templates '(("t" "todo" entry (file "inbox.org") "* TODO %a%?\n%i")
                                          ("e" "event" entry (file+headline "tickler.org" "Events")
                                           "* %^{Description}\n%^{When}T\n%?")
                                          ("r" "reminder" entry (file+headline "tickler.org" "Reminders")
                                           "* %^{What}\nDEADLINE: %^{When}T --%^{Remind before (e.g. 1d, 2w, 3h)|1d}")
                                          ("m" "meeting" entry (file+headline "tickler.org" "Meetings")
                                           "* Meeting with %^{Person}\n%^{When}T\n** Prep\n%?\n** Notes\n\n** Actions\n"))

                  org-refile-targets `((,(expand-file-name "gtd.org" org-directory) :maxlevel . 3))
                  org-stuck-projects '("+LEVEL=2+PROJECT/-DONE" ("NEXT") nil ""))

          (defun my/org-fontify-line-starts (limit)
            "Keep indentation and list bullets fixed pitch up to LIMIT.
          Wrapped lines continue at the column where the text starts, so that
          hard-wrapped and soft-wrapped lines align in a proportional font.
          Also add space above headings, without blank lines in the file."
            (while (< (point) limit)
              (let ((bol (point))
                    (end (if (looking-at (org-item-re))
                             (match-end 0)
                           (skip-chars-forward " \t")
                           (point))))
                (unless (bobp)
                  ;; The newline before a heading is visible even when folded.
                  (if (looking-at-p org-outline-regexp)
                      (put-text-property (1- bol) bol 'line-spacing 0.5)
                    (remove-text-properties (1- bol) bol '(line-spacing nil))))
                (unless (bound-and-true-p org-indent-mode) ; sets its own prefixes
                  (if (= end bol)
                      (remove-text-properties bol (pos-eol) '(wrap-prefix nil))
                    (add-face-text-property bol end 'fixed-pitch)
                    (goto-char end)
                    (put-text-property
                     bol (pos-eol) 'wrap-prefix
                     (propertize (make-string (current-column) ?\s)
                                 'face 'fixed-pitch)))))
              (forward-line))
            nil)

          (font-lock-add-keywords 'org-mode '((my/org-fontify-line-starts)) t)

          ;; Frames named "org-capture-dedicated" and "org-agenda-dedicated" (the
          ;; xmonad scratchpads) show only that buffer and are deleted with it.

          (defun my/org-dedicated-frame-p (kind)
            "Return whether the selected frame is the dedicated org KIND frame."
            (equal (frame-parameter nil 'name) (format "org-%s-dedicated" kind)))

          (add-hook 'org-capture-mode-hook
                    (lambda () (when (my/org-dedicated-frame-p "capture") (delete-other-windows))))
          (add-hook 'org-capture-after-finalize-hook
                    (lambda () (when (my/org-dedicated-frame-p "capture") (delete-frame))))
          (advice-add 'org-agenda-quit :after
                      (lambda () (when (my/org-dedicated-frame-p "agenda") (delete-frame))))
        '';
      };

      org-appear = {
        enable = true;
        hook = [ "(org-mode . org-appear-mode)" ];
        config = ''
          (setopt org-appear-autoemphasis t
                  org-appear-autolinks t
                  org-appear-autokeywords t)
        '';
      };

      org-download = {
        enable = true;
        hook = [ "(dired-mode . org-download-enable)" ];
        command = [
          "org-download-clipboard"
          "org-download-screenshot"
          "org-download-yank"
        ];
        config = ''
          (setopt org-download-method 'attach)
        '';
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
        after = [ "org" ];
        init = ''
          (make-directory (expand-file-name "roam" org-directory) t)
        '';
        bind = {
          "C-c n c" = "org-roam-capture";
          "C-c n f" = "org-roam-node-find";
          "C-c n i" = "org-roam-node-insert";
        };
        config = ''
          (setopt org-roam-directory (expand-file-name "roam" org-directory)
                  org-roam-dailies-directory "daily/"
                  org-roam-dailies-capture-templates '(("d" "default" entry
                                                        "* %?"
                                                        :target (file+head "%<%Y-%m-%d>.org"
                                                                           "#+title: %<%Y-%m-%d>\n"))))

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
