{ config, lib, pkgs, ... }:
{
  programs.emacs.init.prelude = ''
    (defvar org-notes-directory (file-name-as-directory "~/src/org-notes/"))
    (defvar org-notes-gtd-directory (file-name-as-directory (concat org-notes-directory "gtd")))
    (defvar org-notes-gtd-inbox-file (concat org-notes-gtd-directory "inbox.org"))
    (defvar org-notes-gtd-projects-file (concat org-notes-gtd-directory "gtd.org"))
    (defvar org-notes-gtd-habits-file (concat org-notes-gtd-directory "habits.org"))
    (defvar org-notes-gtd-someday-file (concat org-notes-gtd-directory "someday.org"))
    (defvar org-notes-flashcards-file (concat org-notes-gtd-directory "flash-cards.org"))
    (defvar org-notes-bookmarks-file (concat org-notes-directory "bookmarks.org"))
    (defvar org-notes-journal-file (concat org-notes-directory "journal.org"))
    (defvar org-notes-notes-directory (concat org-notes-directory (file-name-as-directory "notes")))
    (defvar org-notes-references-directory (concat org-notes-directory (file-name-as-directory "references")))

    (defun org-notes-display-bookmarks-in-side-window ()
      "Display org-notes bookmarks file in a side window."
      (interactive)
      (select-window
       (display-buffer-in-side-window
        (find-file-noselect org-notes-bookmarks-file) nil)))

    (defun org-notes-open-journal ()
      "Open org-notes journal file."
      (interactive)
      (find-file org-notes-journal-file))

    (defun org-notes-gtd-open-projects ()
      "Open org-notes gtd projects file."
      (interactive)
      (find-file org-notes-gtd-projects-file))

    (defun org-notes-maybe-sync ()
      "Sync org notes if the current buffer is visiting an org file in the org-notes directory."
      (when (and (derived-mode-p 'org-mode)
                 (string-prefix-p (expand-file-name org-notes-directory)
                                  (buffer-file-name)))
        (org-notes-sync)))

    (defun org-notes-sync ()
      "Sync org notes repo with upstream."
      (interactive)
      (let ((default-directory org-notes-directory))
        (git-sync)))

    (defun org-notes-save-and-sync ()
      "Save all org buffers and sync gtd repo."
      (interactive)
      (org-save-all-org-buffers)
      (org-notes-sync))
  '';

  programs.emacs.init.usePackage = {
    biblio = {
      enable = true;
      after = [ "org" ];
      config = ''
        (setq biblio-download-directory
              (file-name-as-directory
               (concat org-notes-references-directory "pdf")))
      '';
    };

    bibtex-completion = {
      enable = true;
      config = ''
        (setq bibtex-completion-bibliography `(,(concat org-notes-references-directory "master.bib")))
        (setq bibtex-completion-library-path `(,(concat org-notes-references-directory "pdf")))
      '';
    };

    citar = {
      enable = true;
      bindLocal.minibuffer-local-map = {
        "M-b" = "citar-insert-preset";
      };
      config = ''
        (setq citar-bibliography  `(,(concat org-notes-references-directory "master.bib")))
        (setq citar-library-paths `(,(concat org-notes-references-directory "pdf")))
        (setq citar-notes-paths   `(,(concat org-notes-references-directory "notes")))

        ;; use org-roam-bibtex
        (setq citar-file-open-note-function 'orb-bibtex-actions-edit-note)

        ;; use icons - https://github.com/bdarcus/citar
        (setq citar-symbols
              `((file . (,(all-the-icons-icon-for-file "a.pdf" :face 'all-the-icons-dred) .
                         ,(all-the-icons-icon-for-file "a.pdf" :face 'citar-icon-dim)))
                (note . (,(all-the-icons-icon-for-file "a.txt") .
                         ,(all-the-icons-icon-for-file "a.txt" :face 'citar-icon-dim)))
                (link . (,(all-the-icons-faicon "external-link-square" :v-adjust 0.02 :face 'all-the-icons-dpurple) .
                         ,(all-the-icons-faicon "external-link-square" :v-adjust 0.02 :face 'citar-icon-dim)))))

        (defface citar-icon-dim
          '((((background dark)) :foreground "${config.lib.stylix.colors.withHashtag.base01}")
            (((background light)) :foreground "${config.lib.stylix.colors.withHashtag.base07}"))
          "Face for obscuring/dimming icons"
          :group 'all-the-icons-faces)
      '';
    };

    citar-org = {
      enable = true;
      demand = true;
      after = [ "org" ];
      bindLocal.org-mode-map = {
        "C-c r" = "org-cite-insert";
      };
      config = ''
        (setq org-cite-global-bibliography `(,(concat org-notes-references-directory "master.bib")))
        (setq org-cite-insert-processor 'citar)
        (setq org-cite-follow-processor 'citar)
        (setq org-cite-activate-processor 'citar)
      '';
    };

    cdlatex = {
      enable = true;
      command = [ "turn-on-cdlatex" ];
    };

    git-sync.enable = true;

    ob-restclient = {
      enable = true;
      after = [ "org" ];
    };

    org = {
      enable = true;
      package = ps: ps.elpaPackages.org;
      bind = {
        "C-c o a" = "org-agenda";
        "C-c o c" = "org-capture";
        "C-c o b" = "org-notes-display-bookmarks-in-side-window";
        "C-c o j" = "org-notes-open-journal";
        "C-c o l" = "org-store-link";
        "C-c o o" = "org-notes-gtd-open-projects";
        "C-c o s" = "org-notes-save-and-sync";
      };
      bindLocal.org-mode-map = {
        "C-c C-x k d" = "org-toggle-link-display";
        "C-c C-x k i" = "org-insert-link";
      };
      hook = [
        "(org-mode . turn-on-visual-line-mode)"
        "(org-mode . turn-on-flyspell)"
        "(org-mode . org-indent-mode)"
        "(after-save . org-notes-maybe-sync)"
        "(org-babel-after-execute . org-redisplay-inline-images)"
      ];
      config = ''
        (setq org-hide-emphasis-markers t)
        (setq org-startup-indented t)
        (setq org-tags-column 0) ; don't try to align tags
        (setq org-todo-keywords '((sequence "TODO" "NEXT" "BLOCKED" "REVIEW" "|" "DONE")))
        (setq org-tag-persistent-alist '(("PROJECT" . ?P) (:startgroup) ("@home" . ?h) ("@work" . ?w)))
        (setq org-tags-sort-function #'string<)
        (setq org-confirm-babel-evaluate nil)
        (setq org-log-into-drawer t)

        (add-to-list 'org-file-apps '("\\.pdf\\'" . emacs))
        (add-to-list 'org-file-apps-gnu '(t . "xdg-open %s")) ; use xdg-open as default (replaces mailcap)
        (add-to-list 'org-modules 'org-habit)

        ;; Display centered dots for list bullets
        (font-lock-add-keywords 'org-mode
                                '(("^ *\\([-]\\) "
                                   (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

        (setq org-format-latex-options (plist-put org-format-latex-options :scale 1.2))

        (org-babel-do-load-languages
         'org-babel-load-languages
         '((calc . t)
           (emacs-lisp . t)
           (restclient . t)))

        (advice-add 'org-edit-src-exit :before (lambda () (ignore-errors (format-all-buffer))))

        ;; Don't require confirmation to run code blocks
        (setq org-confirm-babel-evaluate nil)

        ;; Export settings
        (with-eval-after-load 'ox-latex
          (setq org-latex-listings 'minted)
          (setq org-latex-pdf-process '("latexmk -shell-escape -bibtex -f -pdf -output-directory=%o %f"))
          (add-to-list 'org-latex-packages-alist '("newfloat" "minted")))
      '';
    };

    org-agenda = {
      enable = true;
      after = [ "org" ];
      bindLocal.org-agenda-mode-map = {
        "j" = "evil-next-line";
        "k" = "evil-previous-line";
        "C-u" = "evil-scroll-page-up";
        "C-d" = "evil-scroll-page-down";
        "C-w h" = "evil-window-left";
        "C-w l" = "evil-window-right";
      };
      config = ''
        (setq org-agenda-files
              (list org-notes-gtd-inbox-file
                    org-notes-gtd-projects-file
                    org-notes-gtd-habits-file))
        (setq org-agenda-custom-commands
              '(("i" "Inbox" alltodo "" ((org-agenda-files (list org-notes-gtd-inbox-file))))
                ("p" "Projects" tags "LEVEL=2+PROJECT" ((org-agenda-files (list org-notes-gtd-projects-file))))
                ("n" "Next tasks"  tags-todo "TODO=\"NEXT\"")))
        (setq org-refile-targets
              (let '(targets (append (list org-notes-gtd-someday-file) org-agenda-files))
                `((,targets :maxlevel . 2))))
        (setq org-stuck-projects
              '("LEVEL=2+PROJECT-TODO=DONE"
                ("NEXT")
                nil
                ""))
        (setq org-enforce-todo-dependencies t)
      '';
    };

    org-capture = {
      enable = true;
      after = [ "org" ];
      config = ''
        (setq org-capture-templates
              '(("t" "Todo" entry
                 (file org-notes-gtd-inbox-file)
                 "* TODO %?\n%U\n")
                ("b" "Bookmark" entry
                 (file+headline org-notes-bookmarks-file "Bookmarks")
                 "* [[%^{url}][%?]]\n%U\n")
                ("j" "Journal" entry
                 (file+olp+datetree org-notes-journal-file)
                 "* %?\nEntered on %U\n  %i\n  %a")
                ("f" "Flash card" entry
                 (file+headline org-notes-flashcards-file "Flash cards")
                 "* %?\n:PROPERTIES:\n:ANKI_NOTE_TYPE: %^{Note type|Basic}\n:ANKI_DECK: %^{Deck|Misc}\n:END:\n** Front\n** Back")))
      '';
    };

    org-capture-pop-frame = {
      enable = true;
      config = ''
        ;; Don't show menu bar in new frame
        (add-to-list 'ocpf-frame-parameters '(menu-bar-lines . 0))
      '';
    };

    org-download = {
      enable = true;
      after = [ "org" ];
      bindLocal.org-mode-map = {
        "M-s s" = "org-download-screenshot";
        "M-s y" = "org-download-yank";
      };
      config = lib.mkIf (!pkgs.stdenv.isDarwin) ''
        (setq org-download-screenshot-method "${pkgs.xfce.xfce4-screenshooter}/bin/xfce4-screenshooter --region --mouse --save %s")
      '';
    };

    org-pomodoro = {
      enable = true;
      after = [ "org" ];
      bindLocal.org-agenda-mode-map = {
        p = "org-pomodoro";
      };
    };

    org-ref = {
      enable = true;
      command = [
        "org-ref-bibtex-assoc-pdf-with-entry"
        "org-ref-clean-bibtex-entry"
      ];
    };

    org-roam = {
      enable = true;
      init = ''
        (setq org-roam-v2-ack t)
        (setq org-roam-directory org-notes-notes-directory)
        (setq org-roam-dailies-directory "daily/")
        (setq org-roam-dailies-capture-templates
              '(("d" "default" entry
                 "* %?"
                 :if-new (file+head "%<%Y-%m-%d>.org"
                                    "#+title: %<%Y-%m-%d>\n"))))
      '';
      bind = {
        "C-c n l" = "org-roam-buffer-toggle";
        "C-c n f" = "org-roam-node-find";
        "C-c n i" = "org-roam-node-insert";
        "C-c n c" = "org-roam-capture";
        "C-c n j" = "org-roam-dailies-capture-today";
      };
      config = ''
        (org-roam-db-autosync-mode)
        (require 'org-roam-protocol)
      '';
    };

    org-roam-bibtex = {
      enable = true;
      diminish = [ "org-roam-bibtex-mode" ];
      config = ''
        (org-roam-bibtex-mode)
      '';
    };

    org-roam-graph = {
      enable = true;
      command = [ "org-roam-graph" ];
      bind = {
        "C-c n g" = "org-roam-graph";
      };
    };

    org-roam-ui = {
      enable = true;
      diminish = [
        "org-roam-ui-mode"
        "org-roam-ui-follow-mode"
      ];
      after = [ "org-roam" ];
      command = [ "org-roam-ui-mode" ];
      config = ''
        (setq org-roam-ui-sync-theme t
              org-roam-ui-follow t
              org-roam-ui-update-on-save t
              org-roam-ui-open-on-start t)
      '';
    };

    org-superstar = {
      enable = true;
      hook = [ "(org-mode . org-superstar-mode)" ];
    };

    org-variable-pitch = {
      enable = true;
      config = ''
        (setq org-variable-pitch-fixed-faces nil)
      '';
      hook = [ "(org-mode . org-variable-pitch-minor-mode)" ];
    };

    ox-pandoc = {
      enable = true;
      config = ''
        (setq org-pandoc-options-for-slidy '((mathjax t)))
      '';
    };
  };
}
