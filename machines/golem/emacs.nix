{ config, pkgs, lib, ... }:
let
  emacs = config.programs.emacs.finalPackage;
  nurNoPkgs = import (import ../../overlays/overlays.d/nix/sources.nix).nur { };

  orgProtocolDesktopItem = pkgs.makeDesktopItem rec {
    name = "org-protocol";
    desktopName = name;
    mimeType = "x-scheme-handler/org-protocol";
    exec = "${emacs}/bin/emacsclient %u";
    icon = "emacs";
    type = "Application";
    terminal = "false";
    categories = "System";
  };

in
{
  imports = [ nurNoPkgs.repos.rycee.hmModules.emacs-init ];

  shell.aliases = {
    ec = "${emacs}/bin/emacsclient --tty";
    emacs = "${emacs}/bin/emacsclient --create-frame";
  };

  home.packages = [ orgProtocolDesktopItem ];

  home.sessionVariables.EDITOR = "${emacs}/bin/emacsclient --tty";

  programs.emacs = {
    enable = true;
    package = pkgs.emacsGit;
  };

  programs.emacs.init = {
    enable = true;
    recommendedGcSettings = true;
    earlyInit = ''
      (add-to-list 'load-path "${pkgs.mypkgs.dotfiles}/emacs.d/elisp/")
      (menu-bar-mode -1)
      (scroll-bar-mode -1)
      (tool-bar-mode -1)
      (tooltip-mode -1)

      (set-face-attribute 'default
                          nil
                          :height 110
                          :family "Fira Code")
      (set-face-attribute 'variable-pitch
                          nil
                          :family "DejaVu Sans")
    '';

    prelude = ''
      (setq custom-file "~/.emacs.d/custom.el")

      (setq user-full-name "${config.user.fullName.first} ${config.user.fullName.last}"
            user-mail-address "${config.user.email}")

      ;; Create backup files in system temp directory
      (setq backup-directory-alist
            `((".*" . ,temporary-file-directory)))
      (setq auto-save-file-name-transforms
            `((".*" ,temporary-file-directory t)))

      (setq split-height-threshold 100)

      ;; Prompt for y/n instead of yes/no
      (defalias 'yes-or-no-p 'y-or-n-p)

      (setq-default indent-tabs-mode nil)
      (show-paren-mode 1)

      ;; Always show line and column numbers
      (line-number-mode)
      (column-number-mode)

      ;; Highlight end-of-line whitespace only in prog-mode
      (add-hook 'prog-mode-hook (lambda () (setq-local show-trailing-whitespace t)))

      ;; Note-taking setup
      (defvar org-notes-directory (file-name-as-directory "~/src/org-notes/"))
      (defvar org-notes-gtd-directory (file-name-as-directory (concat org-notes-directory "gtd")))
      (defvar org-notes-gtd-inbox-file (concat org-notes-gtd-directory "inbox.org"))
      (defvar org-notes-gtd-projects-file (concat org-notes-gtd-directory "gtd.org"))
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

    usePackage = {
      undo-tree = {
        enable = true;
        config = "(global-undo-tree-mode)";
      };

      imenu = {
        enable = true;
        bind."C-c i" = "imenu";
      };

      which-key = {
        enable = true;
        init = ''
          (setq which-key-separator " ")
          (setq which-key-prefix-prefix "+")
        '';
        config = "(which-key-mode 1)";
      };

      smartparens.enable = true;

      rainbow-delimiters = {
        enable = true;
        hook = [ "(prog-mode . rainbow-delimiters-mode)" ];
      };

      evil = {
        enable = true;
        init = ''
          (setq evil-want-C-u-scroll t)
          (setq evil-want-integration t)
          (setq evil-want-keybinding nil)
          (setq evil-respect-visual-line-mode t)
          (setq evil-undo-system 'undo-tree)
        '';
        config = "(evil-mode)";
      };

      evil-collection = {
        enable = true;
        after = [ "evil" ];
        config = "(evil-collection-init)";
      };

      evil-escape = {
        enable = true;
        after = [ "evil" ];
        init = ''(setq-default evil-escape-key-sequence "fd")'';
        config = "(evil-escape-mode)";
      };

      evil-smartparens = {
        enable = true;
        after = [ "evil" "smartparens" ];
        hook = [ "(smartparens-enabled . evil-smartparens-mode)" ];
      };

      evil-surround = {
        enable = true;
        after = [ "evil" ];
        config = "(global-evil-surround-mode)";
      };

      doom-themes = {
        enable = true;
        config = ''
          (setq doom-themes-enable-bold t
                doom-themes-enable-italic t)
          (load-theme 'doom-solarized-dark t)

          (require 'doom-themes-ext-visual-bell)
          (doom-themes-visual-bell-config)

          (require 'doom-themes-ext-treemacs)
          (doom-themes-treemacs-config)
          (setq doom-themes-treemacs-theme "doom-colors")

          (require 'doom-themes-ext-org)
          (doom-themes-org-config)
        '';
      };

      all-the-icons.enable = true;

      ivy = {
        enable = true;
        config = "(ivy-mode 1)";
      };

      ivy-hydra = {
        enable = true;
        after = [ "ivy" "hydra" ];
      };

      counsel = {
        enable = true;
        bind = {
          "C-x C-d" = "counsel-dired-jump";
          "C-x C-f" = "counsel-find-file";
          "C-x M-f" = "counsel-fzf";
          "C-x C-r" = "counsel-recentf";
          "C-x C-y" = "counsel-yank-pop";
          "M-x" = "counsel-M-x";
        };
        config = ''
          (setq counsel-fzf-cmd "${pkgs.fzf}/bin/fzf -f \"%s\"")
          (counsel-mode 1)
        '';
      };

      swiper = {
        enable = true;
        bind."C-s" = "swiper-isearch";
      };

      counsel-tramp = {
        enable = true;
        after = [ "tramp" ];
        bind."C-c f" = "counsel-tramp";
      };

      projectile = {
        enable = true;
        bindKeyMap."C-c p" = "projectile-command-map";
        config = ''
          (setq projectile-require-project-root nil)
          (setq projectile-completion-system 'ivy)
          (setq projectile-project-search-path '("~/src/"))
          (projectile-mode 1)
        '';
      };

      counsel-projectile = {
        enable = true;
        after = [ "counsel" "projectile" ];
        config = "(counsel-projectile-mode 1)";
      };

      direnv = {
        enable = true;
        config = "(direnv-mode)";
      };

      ripgrep = {
        enable = true;
        command = [ "ripgrep-regexp" ];
      };

      hl-todo = {
        enable = true;
        hook = [ "(prog-mode . hl-todo-mode)" ];
      };

      treemacs = {
        enable = true;
        bind = {
          "M-0" = "treemacs-select-window";
          "C-x t 1" = "treemacs-delete-other-windows";
          "C-x t t" = "treemacs";
          "C-x t B" = "treemacs-bookmark";
          "C-x t C-t" = "treemacs-find-file";
          "C-x t M-t" = "treemacs-find-tag";
        };
        config =
          ''(setq treemacs-python-executable "${pkgs.python3}/bin/python")'';
      };

      treemacs-evil = {
        enable = true;
        after = [ "treemacs" "evil" ];
      };

      treemacs-projectile = {
        enable = true;
        after = [ "treemacs" "projectile" ];
      };

      treemacs-magit = {
        enable = true;
        after = [ "treemacs" "magit" ];
      };

      company = {
        enable = true;
        command = [ "company-mode" "company-doc-buffer" "global-company-mode" ];
        bindLocal.company-active-map.jk = "company-complete";
        config = "(global-company-mode)";
      };

      format-all = {
        enable = true;
        bind."C-c C-f" = "format-all-buffer";
      };

      magit.enable = true;

      evil-magit = {
        enable = true;
        after = [ "evil" "magit" ];
      };

      gist.enable = true;

      browse-at-remote = {
        enable = true;
        command = [ "browse-at-remote" ];
        bind = { "C-c b" = "browse-at-remote"; };
      };

      kubernetes = {
        enable = true;
        command = [ "kubernetes-overview" ];
      };

      kubernetes-evil = {
        enable = true;
        after = [ "kubernetes" "evil" ];
      };

      flycheck = {
        enable = true;
        init = ''(setq ispell-program-name "aspell")'';
        config = "(global-flycheck-mode t)";
      };

      lsp-mode = {
        enable = true;
        command = [ "lsp" ];
        after = [ "company" "flycheck" ];
        hook = [
          "(lsp-mode . lsp-enable-which-key-integration)"
          "(lsp-mode . lsp-lens-mode)"
        ];
        init = ''
          (setq lsp-keymap-prefix "C-c l")
        '';
        config = ''
          (setq lsp-diagnostics-provider :flycheck)
          (setq lsp-file-watch-threshold 10000)
        '';
      };

      lsp-ui = {
        enable = true;
        command = [ "lsp-ui-mode" ];
      };

      lsp-ivy = {
        enable = true;
        after = [ "lsp-mode" "ivy" ];
        command =
          [ "lsp-ivy-workspace-symbol" "lsp-ivy-global-workspace-symbol" ];
      };

      lsp-treemacs = {
        enable = true;
        after = [ "lsp-mode" "treemacs" ];
        config = "(setq lsp-metals-treeview-show-when-views-received t)"; # XXX
      };

      latex = {
        enable = true;
        package = epkgs: epkgs.auctex;
        mode = [ ''("\\.tex\\'" . latex-mode)'' ];
        config = ''
          (add-to-list 'TeX-view-program-selection '(output-pdf "PDF Tools"))'';
      };

      markdown-mode = {
        enable = true;
        command = [ "markdown-mode" "gfm-mode" ];
        hook = [
          "(markdown-mode . turn-on-visual-line-mode)"
          "(markdown-mode . turn-on-flyspell)"
        ];
        mode = [
          ''("README\\.md\\'" . gfm-mode)''
          ''("\\.md\\'" . markdown-mode)''
          ''("\\.markdown\\'" . markdown-mode)''
        ];
      };

      edit-indirect.enable = true;

      fira-code-mode.enable = true;

      jupyter = {
        enable = true;
        bind = { "C-c j" = "jupyter-run-repl"; };
      };

      go-mode.enable = true;

      json-mode = {
        enable = true;
        hook = [ "(json-mode . flycheck-mode)" ];
      };

      yaml-mode.enable = true;

      restclient.enable = true;

      company-restclient = {
        enable = true;
        after = [ "company" "restclient" ];
      };

      ob-restclient = {
        enable = true;
        after = [ "org-babel" "restclient" ];
      };

      logview.enable = true;

      yasnippet = {
        enable = true;
        config = "(yas-global-mode 1)";
      };

      yasnippet-snippets.enable = true;

      git-sync = {
        enable = true;
        package = epkgs:
          epkgs.trivialBuild {
            pname = "git-sync";
            version = "2020-11-22";
            src = pkgs.writeText "git-sync.el" ''
              ;;; Package --- Summary
              ;;; Commentary:
              ;;; Code:

              (defvar git-sync-command "git-annex add . && git-annex sync")
              (defvar git-sync-buffer-name (concat "*async " git-sync-command "*"))

              (defun git-sync-sentinel (process event)
                "Watches the git-sync PROCESS for an EVENT indicating a successful sync and closes the window."
                (message event)
                (cond ((string-match-p "finished" event)
                       (message (concat git-sync-command " successful"))
                       (kill-buffer git-sync-buffer-name))
                      ((string-match-p "\\(exited\\|dumped\\)" event)
                       (message (concat git-sync-command " failed"))
                       (when
                           (yes-or-no-p
                            (concat "Error running '" git-sync-command "'. Switch to output?"))
                         (switch-to-buffer git-sync-buffer-name)))))

              (defun git-sync ()
                "Run git-sync as an async process."
                (interactive)
                (if (get-buffer git-sync-buffer-name)
                    (message "git sync already in progress (kill the `%s' buffer to reset)" git-sync-buffer-name)
                  (let* ((process (start-process-shell-command
                                   git-sync-command
                                   git-sync-buffer-name
                                   git-sync-command)))
                    (set-process-sentinel process 'git-sync-sentinel))))

              (provide 'git-sync)
              ;;; git-sync.el ends here
            '';
          };
      };

      org = {
        enable = true;
        bind = {
          "C-c o a" = "org-agenda";
          "C-c o c" = "org-capture";
          "C-c o b" = "org-notes-display-bookmarks-in-side-window";
          "C-c o j" = "org-notes-open-journal";
          "C-c o l" = "org-store-link";
          "C-c o o" = "org-notes-gtd-open-projects";
          "C-c o s" = "org-notes-save-and-sync";
          "C-c C-x l" = "org-toggle-link-display";
        };
        hook = [
          "(org-mode . turn-on-visual-line-mode)"
          "(org-mode . turn-on-flyspell)"
          "(after-save . org-notes-maybe-sync)"
          "(org-babel-after-execute . org-redisplay-inline-images)"
        ];
        config = ''
          (require 'git-sync)

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
           '((R          . t)
             (calc       . t)
             (dot        . t)
             (emacs-lisp . t)
             (haskell    . t)
             (jupyter    . t)
             (latex      . t)
             (restclient . t)
             (shell      . t)))

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
          (setq org-agenda-files (list org-notes-gtd-directory))
          (setq org-agenda-custom-commands
                '(("i" "Inbox" alltodo "" ((org-agenda-files (list org-notes-gtd-inbox-file))))
                  ("p" "Projects" tags "LEVEL=2+PROJECT" ((org-agenda-files (list org-notes-gtd-projects-file))))
                  ("n" "Next tasks"  tags-todo "TODO=\"NEXT\"")))
          (setq org-refile-targets '((org-agenda-files :maxlevel . 2)))
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
                '(("t" "Todo entry")
                  ("tt" "Todo" entry
                   (file org-notes-gtd-inbox-file)
                   "* TODO %?\n%U\n")
                  ("tw" "Todo (work)" entry
                   (file org-notes-gtd-inbox-file)
                   "* TODO %? :@work:\n%U\n")
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

      org-evil = {
        enable = true;
        hook = [ "(org-mode . org-evil-mode)" ];
      };

      cdlatex = {
        enable = true;
        hook = [ "(org-mode . turn-on-cdlatex)" ];
      };

      org-superstar = {
        enable = true;
        hook = [ "(org-mode . org-superstar-mode)" ];
      };

      mixed-pitch = {
        enable = true;
        hook = [ "(text-mode . mixed-pitch-mode)" ];
      };

      org-variable-pitch = {
        enable = true;
        config = "(setq org-variable-pitch-fixed-faces nil)";
        hook = [ "(org-mode . org-variable-pitch-minor-mode)" ];
      };

      ox-gfm.enable = true;

      ox-pandoc.enable = true;

      deft = {
        enable = true;
        after = [ "org" ];
        config = ''
          (setq deft-auto-save-interval 0
                deft-recursive t
                deft-use-filter-string-for-filename t
                deft-default-extension "org"
                deft-directory org-notes-notes-directory)
        '';
      };

      pdf-tools = {
        enable = true;
        mode = [ ''("\\.pdf\\'" . pdf-view-mode)'' ];
        hook = [ "(pdf-view-mode . (lambda () (linum-mode -1)))" ];
        config = ''
          (pdf-tools-install)
          (setq pdf-view-use-scaling t)
          (setq-default pdf-view-display-size 'fit-page)
        '';
      };

      org-roam = {
        enable = true;
        after = [ "org" ];
        hook = [ "(after-init . org-roam-mode)" ];
        bindLocal = {
          org-roam-mode-map = {
            "C-c n l" = "org-roam";
            "C-c n f" = "org-roam-find-file";
            "C-c n g" = "org-roam-graph";
          };
          org-mode-map."C-c n i" = "org-roam-insert";
        };
        config = ''
          (setq org-roam-directory org-notes-notes-directory)
          (require 'org-roam-protocol)
        '';
      };

      company-org-roam = {
        enable = true;
        after = [ "company" ];
        config = "(add-to-list 'company-backends 'company-org-roam)";
      };

      org-download = {
        enable = true;
        after = [ "org" ];
        bindLocal.org-mode-map = {
          s-Y = "org-download-screenshot";
          s-y = "org-download-yank";
        };
      };

      org-ref = {
        enable = true;
        after = [ "org" ];
        config = ''
          (setq reftex-default-bibliography
                (list (concat org-notes-references-directory "master.bib")))
          (setq org-ref-bibliography-notes
                (concat org-notes-references-directory "notes.org"))
          (setq org-ref-default-bibliography
                (list (concat org-notes-references-directory "master.bib")))
          (setq org-ref-pdf-directory
                (concat org-notes-references-directory
                        (file-name-as-directory "bibtex-pdfs")))
          (setq org-ref-completion-library 'org-ref-ivy-cite)
        '';
      };

      bibtex-completion = {
        enable = true;
        after = [ "org" ];
        config = ''
          (setq bibtex-completion-bibliography
                (concat org-notes-references-directory "master.bib"))
          (setq bibtex-completion-library-path
                (concat org-notes-references-directory "bibtex-pdfs"))
          (setq bibtex-completion-notes-path
                (concat org-notes-references-directory "helm-bibtex-notes"))
        '';
      };

      ivy-bibtex = {
        enable = true;
        bind."C-c r" = "ivy-bibtex";
      };

      org-roam-bibtex = {
        enable = true;
        hook = [ "(org-roam-mode . org-roam-bibtex-mode)" ];
        bindLocal.org-mode-map."C-c n a" = "org-note-actions";
      };

      biblio = {
        enable = true;
        after = [ "org" ];
        config = ''
          (setq biblio-download-directory
                (file-name-as-directory
                 (concat org-notes-references-directory "bibtex-pdfs")))
        '';
      };

      org-pomodoro = {
        enable = true;
        after = [ "org-agenda" ];
        bindLocal.org-agenda-mode-map.p = "org-pomodoro";
      };

      erc = {
        enable = true;
        command = [ "erc" ];
        bind."C-c e" = "erc-freenode";
        config = ''
          (setq erc-prompt-for-password nil) ; get login from ~/.authinfo.gpg
          (setq erc-hide-list '("JOIN" "PART" "QUIT"))
          (setq erc-autojoin-channels-alist
                '(("#emacs"
                   "#haskell"
                   "#nixos"
                   "#org-mode"
                   "#python"
                   "freenode.net")))
          (setq erc-autojoin-timing 'ident)

          (add-to-list 'erc-modules 'notifications)
          (add-to-list 'erc-modules 'spelling)
          (erc-update-modules)

          (defun erc-freenode ()
            "Connect to freenode with ERC."
            (interactive)
            (erc :server "irc.freenode.net" :port 6667 :nick "mcwitt"))
        '';
      };

      erc-hl-nicks = {
        enable = true;
        after = [ "erc" ];
      };

      erc-image = {
        enable = true;
        after = [ "erc" ];
      };

      scala-mode = {
        enable = true;
        mode = [ ''"\\.s\\(cala\\|bt\\)$"'' ];
        hook = [ "(scala-mode . lsp)" ];
      };

      sbt-mode = {
        enable = true;
        command = [ "sbt-start" "sbt-command" ];
        config = ''
          ;; WORKAROUND: https://github.com/ensime/emacs-sbt-mode/issues/31
          ;; allows using SPACE when in the minibuffer
          (substitute-key-definition
           'minibuffer-complete-word
           'self-insert-command
           minibuffer-local-completion-map)
          ;; sbt-supershell kills sbt-mode:  https://github.com/hvesalai/emacs-sbt-mode/issues/152
          (setq sbt:program-options '("-Dsbt.supershell=false"))
        '';
      };

      lsp-metals.enable = true;

      posframe.enable = true;

      dap-mode = {
        enable = true;
        hook = [ "(lsp-mode . dap-mode)" "(lsp-mode . dap-ui-mode)" ];
      };

      agda2-mode = {
        enable = true;
        mode = [ ''"\\.l?agda\\'" "\\.lagda.md\\'"'' ];
      };

      pinentry = {
        enable = true;
        config = "(pinentry-start)";
      };

      ivy-pass = {
        enable = true;
        bind."C-c w" = "ivy-pass";
      };

      esup = {
        enable = true;
        command = [ "esup" ];
      };

      ess.enable = true;
    };
  };

  services.emacs.enable = true;
}
