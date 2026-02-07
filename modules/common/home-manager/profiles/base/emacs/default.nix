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
    ./citar.nix
    ./claude-code-ide.nix
    ./consult.nix
    ./corfu.nix
    ./dape.nix
    ./embark.nix
    ./forge.nix
    ./format-all.nix
    ./jupyter.nix
    ./org.nix
    # ./org-latex-preview.nix
    ./snippets.nix
    ./theme.nix
    ./treesit.nix
    ./vertico.nix
  ];

  config = lib.mkIf config.profiles.base.enable {

    programs.emacs = {
      enable = true;
      package = pkgs.emacs-unstable;
      overrides = _: super: {
        elfeed = super.elfeed.overrideAttrs (_: {
          # Include elfeed-web in the package
          postInstall = ''
            cp -r $src/web $out/share/emacs/site-lisp/elpa/elfeed-*
          '';
        });
      };
    };

    programs.emacs.init = {
      enable = true;

      # Generate (and native-compile) a package-quickstart.el file with package autoloads
      packageQuickstart = true;

      recommendedGcSettings = true;

      earlyInit = ''
        (menu-bar-mode -1)
        (scroll-bar-mode -1)
        (tool-bar-mode -1)
        (tooltip-mode -1)
      '';

      prelude = ''
        (setopt package-archives nil) ; make package archives unavailable (use Nix)
        (setopt custom-file "~/.emacs.d/custom.el")
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
        config = ''
          (setopt dired-vc-rename-file t)
        '';
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
          (setopt beacon-color "${config.lib.stylix.colors.withHashtag.orange}")
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
          "M-j" = "code-cells-forward-cell";
          "M-k" = "code-cells-backward-cell";
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
          (add-hook 'eglot-managed-mode-hook
                    (lambda ()
                      ;; https://github.com/joaotavora/eglot/discussions/898
                      ;; Show flymake diagnostics first.
                      (setq eldoc-documentation-functions
                            (cons #'flymake-eldoc-function
                                  (remove #'flymake-eldoc-function eldoc-documentation-functions)))
                      ;; Show all eldoc feedback.
                      (setq eldoc-documentation-strategy #'eldoc-documentation-compose)

                      ;; Enable semantic highlighting if supported
                      (eglot-semantic-tokens-mode 1)))
        '';
      };

      eldoc = {
        enable = true;
        config = ''
          ;; Replace HTML escapes in eldoc when using eglot
          ;; https://emacs.stackexchange.com/a/82952

          (defvar my/eldoc-html-patterns
            '(("&nbsp;" " ")
              ("&lt;" "<")
              ("&gt;" ">")
              ("&amp;" "&")
              ("&quot;" "\"")
              ("&apos;" "'"))
            "List of (PATTERN . REPLACEMENT) to replace in eldoc output.")

          (defun my/string-replace-all (patterns in-string)
            "Replace all cars from PATTERNS in IN-STRING with their pair."
            (mapc (lambda (pattern-pair)
                    (setq in-string
                          (string-replace (car pattern-pair) (cadr pattern-pair) in-string)))
                  patterns)
            in-string)

          (defun my/eldoc-preprocess (orig-fun &rest args)
            "Preprocess the docs to be displayed by eldoc to replace HTML escapes."
            (let ((doc (car args)))
              ;; The first argument is a list of (STRING :KEY VALUE ...) entries
              ;; we replace the text in each such string
              ;; see docstring of `eldoc-display-functions'
              (when (listp doc)
                (setq doc (mapcar
                           (lambda (doc) (cons
                                          (my/string-replace-all my/eldoc-html-patterns (car doc))
                                          (cdr doc)))
                           doc
                           ))
                )
              (apply orig-fun (cons doc (cdr args)))))

          (advice-add 'eldoc-display-in-buffer :around #'my/eldoc-preprocess)
        '';
      };

      elfeed = {
        enable = true;
        bind = {
          "C-x w" = "elfeed";
        };
      };

      elec-pair = {
        enable = true;
        hook = [ "(prog-mode . electric-pair-mode)" ];
        config = ''
          (setopt electric-pair-skip-whitespace-chars '(9 32)) ; relative to default, don't skip newlines
        '';
      };

      emacs = {
        enable = true;
        config = ''
          ;; https://github.com/emacsmirror/undo-fu?tab=readme-ov-file#undo-limits
          (setopt undo-limit 67108864) ; 64mb.
          (setopt undo-strong-limit 100663296) ; 96mb.
          (setopt undo-outer-limit 1006632960) ; 960mb.


          (setopt user-full-name "Matt Wittmann"
                  user-mail-address "mcwitt@gmail.com")

          ;; Create backup files in system temp directory
          (setopt backup-directory-alist
                  `((".*" . ,temporary-file-directory)))
          (setopt auto-save-file-name-transforms
                  `((".*" ,temporary-file-directory t)))

          ;; Always split horizontally (i.e. vertical stack)
          (setopt split-width-threshold nil)

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

          ;; Open URLs with Chromium
          (setopt browse-url-browser-function 'browse-url-chromium)

          ;; Enable smooth scrolling
          ;; (pixel-scroll-mode)  ; disable for now, seems to interact poorly with breadcrumb-mode
        '';
      };

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
          (setopt evil-want-keybinding nil)
          (setopt evil-want-integration t)
          (setopt evil-respect-visual-line-mode t)
        '';
        config = ''
          (setopt evil-undo-system 'undo-fu)

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

        # NOTE: mkAfter ensures that we enable the mode after
        # configuration of e.g. frames-only-mode-use-window-functions
        # is done
        config = lib.mkAfter ''
          (frames-only-mode 1)
        '';
      };

      gist.enable = true;

      git-auto-commit-mode = {
        enable = true;
        config = ''
          (setopt gac-debounce-interval 60)
          (add-to-list 'safe-local-variable-values '(gac-automatically-push-p t))
        '';
      };

      gptel = {
        enable = true;
        bind = {
          "C-c RET" = "gptel-send";
        };
        config = ''
          (setopt gptel-default-mode #'org-mode)
        '';
      };

      hl-todo = {
        enable = true;
        hook = [ "(prog-mode . hl-todo-mode)" ];
      };

      ispell = {
        enable = true;
        config = ''
          (setopt ispell-program-name "aspell")
          (setopt ispell-alternate-dictionary "${pkgs.scowl}/share/dict/words.txt")
        '';
      };

      json-mode.enable = true;

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
        config = ''
          (setopt completion-styles '(orderless)
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
        hook = [
          ''
            (pdf-view-mode . (lambda ()
                               (setq-local auto-revert-interval 1)
                               (auto-revert-mode 1)

                               ;; https://github.com/vedang/pdf-tools?tab=readme-ov-file#display-line-numbers-mode
                               (display-line-numbers-mode -1)))
          ''
        ];
        config = ''
          (pdf-tools-install t t)
          (setopt pdf-view-use-scaling t)
          (setq-default pdf-view-display-size 'fit-page)
          (add-to-list 'revert-without-query "\\.pdf\\'")
        '';
      };

      rainbow-delimiters = {
        enable = true;
        hook = [ "(prog-mode . rainbow-delimiters-mode)" ];
      };

      recentf = {
        enable = true;
        config = ''
          (setopt recentf-max-saved-items 50)
          (recentf-mode t)
        '';
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
      };

      yaml-mode.enable = true;

      yasnippet = {
        enable = true;
        config = ''
          (yas-global-mode 1)
        '';
        diminish = [ "yas-minor-mode" ];
      };

      yasnippet-snippets.enable = true;
    };
  };
}
