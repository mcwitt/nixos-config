{
  config,
  lib,
  nurNoPkgs,
  pkgs,
  ...
}:
{

  imports = [
    nurNoPkgs.repos.rycee.hmModules.emacs-init
    ./consult.nix
    ./corfu.nix
    ./embark.nix
    ./forge.nix
    ./format-all.nix
    ./jupyter.nix
    ./org.nix
    ./theme
    ./vertico.nix
  ];

  config = lib.mkIf config.profiles.base.enable {

    home.packages = [ pkgs.emacs-all-the-icons-fonts ];

    programs.emacs.package = pkgs.emacs-unstable;

    programs.emacs.extraPackages = epkgs: [ epkgs.treesit-grammars.with-all-grammars ];

    programs.emacs.overrides = _: _: {
      eglot = null; # use built-in package
    };

    programs.emacs.init = {
      enable = true;

      packageQuickstart = true; # ensure package autoloads are available

      recommendedGcSettings = true;

      earlyInit = ''
        (menu-bar-mode -1)
        (scroll-bar-mode -1)
        (tool-bar-mode -1)
        (tooltip-mode -1)
      '';

      prelude = ''
        ;; Make package archives unavailable (use Nix)
        (setq package-archives nil)

        (setq custom-file "~/.emacs.d/custom.el")

        (setq user-full-name "Matt Wittmann"
              user-mail-address "mcwitt@gmail.com")

        ;; Create backup files in system temp directory
        (setq backup-directory-alist
              `((".*" . ,temporary-file-directory)))
        (setq auto-save-file-name-transforms
              `((".*" ,temporary-file-directory t)))

        ;; Always split horizontally (i.e. vertical stack)
        (setq split-width-threshold nil)

        ;; Prompt for y/n instead of yes/no
        (defalias 'yes-or-no-p 'y-or-n-p)

        ;; Indent with spaces
        (setq-default indent-tabs-mode nil)

        ;; Highlight matching parens
        (show-paren-mode 1)

        ;; Don't insert a double space between sentences when filling
        (setq-default sentence-end-double-space nil)

        ;; Always show line and column numbers
        (line-number-mode)
        (column-number-mode)

        ;; Highlight end-of-line whitespace only in prog-mode
        (add-hook 'prog-mode-hook (lambda () (setq-local show-trailing-whitespace t)))

        ;; Controls level of treesit syntax highlighting
        (setq treesit-font-lock-level 3)

        ;; Open URLs with Chromium
        (setq browse-url-browser-function 'browse-url-chromium)

        ;; Enable smooth scrolling
        ;; (pixel-scroll-mode)  ; disable for now, seems to interact poorly with breadcrumb-mode
      '';
    };

    programs.emacs.init.usePackage = {

      all-the-icons = {
        enable = true;
        config = ''
          (setq all-the-icons-spacer " ")
        '';
      };

      all-the-icons-completion = {
        enable = true;
        config = ''
          (all-the-icons-completion-mode)
        '';
      };

      all-the-icons-dired = {
        enable = true;
        hook = [ "(dired-mode . all-the-icons-dired-mode)" ];
      };

      autorevert = {
        enable = true;
        diminish = [ "auto-revert-mode" ];
      };

      ace-window = {
        enable = true;
        extraConfig = ''
          :bind* (("C-c w" . ace-window)
                  ("M-o" . ace-window))
        '';
      };

      auctex.enable = true;

      avy = {
        enable = true;
        demand = true;
        bind = {
          "C-:" = "avy-goto-char";
          "C-\"" = "avy-goto-line";
          "M-g w" = "avy-goto-word-or-subword-1";
        };
      };

      beacon = {
        enable = true;
        diminish = [ "beacon-mode" ];
        config = ''
          (setq beacon-color "${config.lib.stylix.colors.withHashtag.orange}")
          (beacon-mode 1)
        '';
      };

      breadcrumb = {
        enable = true;
        config = ''
          ;; Workaround for slow performance when visiting files in /nix/store
          ;; https://github.com/joaotavora/breadcrumb/issues/41
          (defun my/buffer-in-nix-store-p ()
            "Return whether the current buffer's file path starts with /nix/store."
            (let ((file (buffer-file-name)))
              (and file (string-prefix-p "/nix/store" file))))

          (defun my/disable-breadcrumb-local-mode-in-nix-store-buffer ()
            "Disables breadcrumb-local-mode if current buffer's file path starts with /nix/store"
            (when (my/buffer-in-nix-store-p) (breadcrumb-local-mode -1)))

          (add-hook 'find-file-hook #'my/disable-breadcrumb-local-mode-in-nix-store-buffer)

          ;; Enable breadcrumb globally
          (breadcrumb-mode)
        '';
      };

      browse-at-remote = {
        enable = true;
        command = [ "browse-at-remote" ];
        bind = {
          "C-c B" = "browse-at-remote";
        };
      };

      calc = {
        enable = true;
        config = ''
          (defun my/dedicated-full-calc-frame-p ()
            "Return whether the current frame is a dedicated full-calc frame."
            (equal (frame-parameter nil 'name) "full-calc-dedicated"))

          (defun my/dedicated-full-calc-frame-cleanup (&rest _args)
            "Delete the frame if it is a dedicated full-calc frame."
            (when (my/dedicated-full-calc-frame-p)
              (delete-frame)))

          (advice-add #'calc-quit :after #'my/dedicated-full-calc-frame-cleanup)
        '';
      };

      cape = {
        enable = true;

        init = ''
          ;; Add `completion-at-point-functions', used by `completion-at-point'.
          ;; NOTE: The order matters!
          (add-to-list 'completion-at-point-functions #'cape-dabbrev)
          (add-to-list 'completion-at-point-functions #'cape-file)
          ;;(add-to-list 'completion-at-point-functions #'cape-elisp-block)
          ;;(add-to-list 'completion-at-point-functions #'cape-history)
          ;;(add-to-list 'completion-at-point-functions #'cape-keyword)
          ;;(add-to-list 'completion-at-point-functions #'cape-tex)
          ;;(add-to-list 'completion-at-point-functions #'cape-sgml)
          ;;(add-to-list 'completion-at-point-functions #'cape-rfc1345)
          ;;(add-to-list 'completion-at-point-functions #'cape-abbrev)
          ;;(add-to-list 'completion-at-point-functions #'cape-dict)
          ;;(add-to-list 'completion-at-point-functions #'cape-symbol)
          ;;(add-to-list 'completion-at-point-functions #'cape-line)
        '';

        bind = {
          "C-c p p" = "completion-at-point"; # capf
          "C-c p t" = "complete-tag"; # etags
          "C-c p d" = "cape-dabbrev"; # or dabbrev-completion
          "C-c p h" = "cape-history";
          "C-c p f" = "cape-file";
          "C-c p k" = "cape-keyword";
          "C-c p s" = "cape-symbol";
          "C-c p a" = "cape-abbrev";
          "C-c p l" = "cape-line";
          "C-c p w" = "cape-dict";
          "C-c p \\\\" = "cape-tex";
          "C-c p &" = "cape-sgml";
          "C-c p r" = "cape-rfc1345";
        };
      };

      code-cells = {
        enable = true;
        bindLocal.code-cells-mode-map = {
          "C-c C-c" = "code-cells-eval";
        };
        config = ''
          ;; https://github.com/astoff/code-cells.el#speed-keys
          (let ((map code-cells-mode-map))
            (define-key map [remap evil-search-next] (code-cells-speed-key 'code-cells-forward-cell)) ;; n
            (define-key map [remap evil-paste-after] (code-cells-speed-key 'code-cells-backward-cell)) ;; p
            (define-key map [remap evil-backward-word-begin] (code-cells-speed-key 'code-cells-eval-above)) ;; b
            (define-key map [remap evil-forward-word-end] (code-cells-speed-key 'code-cells-eval)) ;; e
            (define-key map [remap evil-jump-forward] (code-cells-speed-key 'outline-cycle))) ;; TAB
        '';
      };

      command-log-mode = {
        enable = true;
        command = [ "command-log-mode" ];
      };

      copilot = {
        enable = true;
        command = [ "copilot-mode" ];
        bindLocal.copilot-completion-map = {
          "<tab>" = "copilot-accept-completion";
          "TAB" = "copilot-accept-completion";
          "C-TAB" = "copilot-accept-completion-by-word";
          "C-<tab>" = "copilot-accept-completion-by-word";
          "C-RET" = "copilot-accept-completion-by-line";
          "C-<return>" = "copilot-accept-completion-by-line";
          "M-<up>" = "copilot-previous-completion";
          "M-<down>" = "copilot-next-completion";
          "C-g" = "copilot-clear-overlay";
        };
        config = ''
          (setq copilot-node-executable "${pkgs.nodejs}/bin/node")

          ;; needed because package uses executable-find to locate npm
          ;; https://github.com/copilot-emacs/copilot.el/blob/4e203efaa1f4047c800a026ba496d3bda8b67119/copilot.el#L1001
          (add-to-list 'exec-path "${pkgs.nodejs}/bin/")
        '';
      };

      copilot-chat.enable = true;

      csv-mode.enable = true;

      edit-indirect.enable = true;

      eglot = {
        enable = true;
        command = [ "eglot" ];

        bindLocal.eglot-mode-map = {
          "C-c l a" = "eglot-code-actions";
          "C-c l h" = "eglot-inlay-hints-mode";
          "C-c l r" = "eglot-rename";
        };

        config = ''
          ;; https://github.com/joaotavora/eglot/discussions/898
          (add-hook 'eglot-managed-mode-hook
                    (lambda ()
                      ;; Show flymake diagnostics first.
                      (setq eldoc-documentation-functions
                            (cons #'flymake-eldoc-function
                                  (remove #'flymake-eldoc-function eldoc-documentation-functions)))
                      ;; Show all eldoc feedback.
                      (setq eldoc-documentation-strategy #'eldoc-documentation-compose)))
        '';
      };

      eldoc.enable = true;

      electric-pair-mode = {
        enable = true;
        hook = [ "(prog-mode . electric-pair-mode)" ];
        config = ''
          (setq electric-pair-skip-whitespace-chars '(9 32)) ; relative to default, don't skip newlines
        '';
      };

      emacs.enable = true;

      envrc = {
        enable = true;
        demand = true;
        bindLocal.envrc-mode-map = {
          "C-c e" = "envrc-command-map";
        };
        config = ''
          (envrc-global-mode)
        '';
      };

      esup = {
        enable = true;
        command = [ "esup" ];
      };

      evil = {
        enable = true;
        init = ''
          (setq evil-want-integration t)
          (setq evil-want-keybinding nil)
          (setq evil-respect-visual-line-mode t)
          (setq evil-undo-system 'undo-fu)
        '';
        config = ''
          (evil-mode)
        '';
      };

      evil-collection = {
        enable = true;
        after = [ "evil" ];
        config = ''
          (evil-collection-init)
        '';
        diminish = [ "evil-collection-unimpaired-mode" ];
      };

      evil-commentary = {
        enable = true;
        after = [ "evil" ];
        config = ''
          (evil-commentary-mode)
        '';
        diminish = [ "evil-commentary-mode" ];
      };

      evil-escape = {
        enable = true;
        init = ''
          (setq-default evil-escape-key-sequence "fd")
          (setq-default evil-escape-delay 0.2)
        '';
        config = ''
          (evil-escape-mode)
        '';
        diminish = [ "evil-escape-mode" ];
      };

      evil-numbers = {
        enable = true;
        bind = {
          "C-c +" = "evil-numbers/inc-at-pt";
          "C-c -" = "evil-numbers/dec-at-pt";
        };
      };

      evil-surround = {
        enable = true;
        after = [ "evil" ];
        config = ''
          (global-evil-surround-mode)
        '';
      };

      evil-string-inflection.enable = true;

      expand-region = {
        enable = true;
        bind = {
          "C-=" = "er/expand-region";
        };
      };

      flymake = {
        enable = true;
        bind = {
          "M-n" = "flymake-goto-next-error";
          "M-p" = "flymake-goto-prev-error";
        };
      };

      flymake-shellcheck = {
        enable = true;
        hook = [
          ''
            (sh-mode . (lambda ()
                         (flymake-shellcheck-load)
                         (flymake-mode)))
          ''
        ];
      };

      frames-only-mode = {
        enable = true;
        config = ''
          (frames-only-mode 1)
        '';
      };

      gist.enable = true;

      git-auto-commit-mode = {
        enable = true;
        config = ''
          (setq gac-debounce-interval 60)
          (add-to-list 'safe-local-variable-values '(gac-automatically-push-p t))
        '';
      };

      gptel = {
        enable = true;
        bind = {
          "C-c RET" = "gptel-send";
        };
        custom = {
          gptel-default-mode = "#'org-mode";
          gptel-crowdsourced-prompts-file = ''"${
            pkgs.fetchFromGitHub {
              owner = "f";
              repo = "awesome-chatgpt-prompts";
              rev = "06708da20073a038a3970a1ebac25bd643afeefa";
              hash = "sha256-LX1ShrR9D5tW72zD4BlRIbwopjCkVeKcRgT/eHQNcCU=";
            }
          }/prompts.csv"'';
        };
      };

      hl-todo = {
        enable = true;
        hook = [ "(prog-mode . hl-todo-mode)" ];
      };

      ispell = {
        enable = true;
        custom = {
          ispell-program-name = ''"aspell"'';
          ispell-alternate-dictionary = ''"${pkgs.scowl}/share/dict/words.txt"'';
        };
      };

      json-mode = {
        enable = true;
        init = ''
          (add-to-list 'major-mode-remap-alist '(json-mode . json-ts-mode))
        '';
      };

      ligature = {
        enable = true;
        config = ''
          (global-ligature-mode t)
        '';
      };

      logview = {
        enable = true;
        command = [ "logview-mode" ];
        mode = [ ''("\\.log\\(?:\\.[0-9]+\\)?\\'" . logview-mode)'' ];
      };

      magit = {
        enable = true;
        bind = {
          "C-x g" = "magit-status";
          "C-x M-g" = "magit-dispatch";
          "C-c M-g" = "magit-file-dispatch";
        };
      };

      magit-extras = {
        enable = true;
        demand = true; # e.g. to add entry to `project-switch-commands'
      };

      marginalia = {
        enable = true;
        bind."M-A" = "marginalia-cycle";
        bindLocal.minibuffer-local-map."M-A" = "marginalia-cycle";
        init = ''
          (marginalia-mode)
        '';
      };

      orderless = {
        enable = true;
        init = ''
          (setq completion-styles '(orderless)
                completion-category-defaults nil
                completion-category-overrides '((file (styles . (partial-completion)))))
        '';
      };

      pinentry = {
        enable = true;
        config = ''
          (pinentry-start)
        '';
      };

      pdf-tools = {
        enable = true;
        hook = [ "(pdf-view-mode . (lambda () (linum-mode -1)))" ];
        config = ''
          (pdf-tools-install t t)
          (setq pdf-view-use-scaling t)
          (setq-default pdf-view-display-size 'fit-page)
        '';
      };

      rainbow-delimiters = {
        enable = true;
        hook = [ "(prog-mode . rainbow-delimiters-mode)" ];
      };

      restclient = {
        enable = true;
        command = [ "restclient-mode" ];
      };

      savehist = {
        enable = true;
        config = ''
          (savehist-mode)
        '';
      };

      subword.enable = true;

      string-inflection = {
        enable = true;
        bind = {
          "C-c u" = "string-inflection-all-cycle";
        };
      };

      undo-fu.enable = true;

      undo-fu-session = {
        enable = true;
        config = ''
          (undo-fu-session-global-mode)
        '';
      };

      vundo = {
        enable = true;
        bind = {
          "C-x u" = "vundo";
        };
        bindLocal.vundo-mode-map = {
          "h" = "vundo-backward";
          "l" = "vundo-forward";
          "k" = "vundo-previous";
          "j" = "vundo-next";
        };
        config = ''
          (setq vundo-glyph-alist vundo-unicode-symbols)
        '';
      };

      yaml-mode = {
        enable = true;
        hook = [ "(yaml-mode . (lambda () (mixed-pitch-mode -1)))" ];
        init = ''
          (add-to-list 'major-mode-remap-alist '(yaml-mode . yaml-ts-mode))
        '';
      };

      yasnippet = {
        enable = true;
        config = ''
          (yas-global-mode 1)
        '';
      };

      yasnippet-snippets.enable = true;
    };
  };
}
