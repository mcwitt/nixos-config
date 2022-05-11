{ lib, pkgs, ... }: {

  imports = [
    ./completion.nix
    ./emoji.nix
    ./erc.nix
    ./evil.nix
    ./jupyter.nix
    ./lsp.nix
    ./org.nix
    ./pdf-tools.nix
    ./tree-sitter.nix
    ./treemacs.nix
  ];

  programs.emacs.overrides = _: prev: {
    # https://github.com/NixOS/nixpkgs/issues/172178
    pdf-tools = prev.pdf-tools.overrideAttrs (_: {
      CXXFLAGS = "-std=c++17";
    });
  };

  programs.emacs.package = lib.mkDefault pkgs.emacsNativeComp;

  programs.emacs.init = {

    enable = true;
    recommendedGcSettings = true;

    earlyInit = ''
      (menu-bar-mode -1)
      (scroll-bar-mode -1)
      (tool-bar-mode -1)
      (tooltip-mode -1)
    '';

    prelude = ''
      (setq custom-file "~/.emacs.d/custom.el")

      (setq user-full-name "Matt Wittmann"
            user-mail-address "mcwitt@gmail.com")

      ;; Create backup files in system temp directory
      (setq backup-directory-alist
            `((".*" . ,temporary-file-directory)))
      (setq auto-save-file-name-transforms
            `((".*" ,temporary-file-directory t)))

      (setq split-height-threshold 100)

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
      (setq browse-url-browser-function 'browse-url-chromium)

      ;; Make package archives unavailable (use Nix)
      (setq package-archives nil)
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
      command = [ "all-the-icons-dired-mode" ];
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

    avy = {
      enable = true;
      demand = true;
      bind = {
        "C-:" = "avy-goto-char";
        "M-g w" = "avy-goto-word-or-subword-1";
      };
    };

    beacon = {
      enable = true;
      diminish = [ "beacon-mode" ];
      config = ''
        (setq beacon-color "#b58900")
        (beacon-mode 1)
      '';
    };

    browse-at-remote = {
      enable = true;
      command = [ "browse-at-remote" ];
      bind = { "C-c B" = "browse-at-remote"; };
    };

    company = {
      enable = true;
      diminish = [ "company-mode" ];
      command = [ "company-mode" "company-doc-buffer" "global-company-mode" ];
      bindLocal.company-active-map = {
        "jk" = "company-complete";
      };
      config = "(global-company-mode)";
    };

    company-box = {
      enable = true;
      diminish = [ "company-box-mode" ];
      hook = [ "(company-mode . company-box-mode)" ];
      config = ''
        (setq company-box-icons-alist 'company-box-icons-all-the-icons)
      '';
    };

    company-restclient = {
      enable = true;
      after = [ "company" "restclient" ];
    };

    csv-mode = {
      enable = true;
      mode = [ ''"\\.[Cc][Ss][Vv]\\'"'' ];
      hook = [ "(csv-mode . csv-align-mode)" ];
    };

    dockerfile-mode = {
      enable = true;
      mode = [ ''"Dockerfile\\'"'' ];
    };

    edit-indirect.enable = true;

    eldoc = {
      enable = true;
      diminish = [ "eldoc-mode" ];
      command = [ "eldoc-mode" ];
    };

    elisp-format = {
      enable = true;
      command = [ "elisp-format-region" "elisp-format-buffer" ];
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

    expand-region = {
      enable = true;
      bind = { "C-=" = "er/expand-region"; };
    };

    fira-code-mode = {
      enable = true;
      diminish = [ "fira-code-mode" ];
      command = [ "fira-code-mode" ];
    };

    flycheck = {
      enable = true;
      demand = true;
      diminish = [ "flycheck-mode" ];
      bind = {
        "M-n" = "flycheck-next-error";
        "M-p" = "flycheck-previous-error";
      };
      config = ''
        (setq flycheck-check-syntax-automatically '(save idle-change new-line))
        (setq flycheck-idle-change-delay 0.5)
        (setq flycheck-checker-error-threshold nil)
        (global-flycheck-mode t)
      '';
    };

    format-all = {
      enable = true;
      diminish = [ "format-all-mode" ];
      hook = [
        "(prog-mode . format-all-mode)"
        "(format-all-mode . format-all-ensure-formatter)"
      ];
    };

    forge = {
      enable = true;
      after = [ "magit" ];
      config = ''
        (setq forge-add-default-bindings nil)
      '';
    };

    frames-only-mode = {
      enable = true;
      config = ''
        ;; Prevent error when attempting to open in a new frame
        ;; Must run before enabling mode
        (add-to-list 'frames-only-mode-use-window-functions #'embark-act)

        (frames-only-mode 1)
      '';
    };

    gist.enable = true;

    groovy-mode = {
      enable = true;
      mode = [
        ''"\\.gradle\\'"''
        ''"\\.groovy\\'"''
        ''"Jenkinsfile\\'"''
      ];
    };

    hl-todo = {
      enable = true;
      hook = [ "(prog-mode . hl-todo-mode)" ];
    };

    imenu = {
      enable = true;
      bind = { "C-c i" = "imenu"; };
    };

    ispell = {
      enable = true;
      config = ''
        (setq ispell-program-name "aspell")
      '';
    };

    json-mode = {
      enable = true;
      mode = [ ''"\\.json\\'"'' ];
      hook = [ "(json-mode . flycheck-mode)" ];
    };

    kubernetes = {
      enable = true;
      command = [ "kubernetes-overview" ];
    };

    lightswitch = {
      enable = true;
      package = epkgs:
        epkgs.trivialBuild {
          pname = "lightswitch";
          version = "2021-06-07";
          src = pkgs.writeText "lightswitch.el" ''
            ;;; lightswitch.el --- Switch between light and dark themes  -*- lexical-binding: t; -*-

            ;; Copyright (C) 2021 Matt Wittmann

            ;; Author: Matt Wittmann <mcwitt@gmail.com>
            ;; Keywords: themes
            ;; Package-Requires: ((solarized "1.3.1"))
            ;; Version: 1.0.0

            ;;; Commentary:

            ;;; Code:

            (defgroup lightswitch nil "Lightswitch theme switcher."
              :group 'theme
              :tag "Lightswitch")

            (defcustom lightswitch-light-theme 'solarized-light
              "Light theme to use."
              :type 'symbol
              :group 'lightswitch)

            (defcustom lightswitch-dark-theme 'solarized-dark
              "Dark theme to use."
              :type 'symbol
              :group 'lightswitch)

            (defadvice load-theme (before lightswitch--disable-custom-themes activate)
              "Disable all custom themes."
              (mapc #'disable-theme custom-enabled-themes))

            (defun lightswitch-toggle ()
              "Toggle between light and dark themes."
              (interactive)
              (if (member lightswitch-light-theme custom-enabled-themes)
                  (load-theme lightswitch-dark-theme t)
                (load-theme lightswitch-light-theme t)))

            (provide 'lightswitch)
            ;;; lightswitch.el ends here
          '';
        };
      bind = {
        "C-c t" = "lightswitch-toggle";
      };
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

    mixed-pitch = {
      enable = true;
      hook = [ "(text-mode . mixed-pitch-mode)" ];
      diminish = [ "mixed-pitch-mode" ];
    };

    multiple-cursors = {
      enable = true;
      bind = {
        "C-x | e" = "mc/edit-lines";
        "C-x | d" = "mc/mark-all-dwim";
      };
    };

    pinentry = {
      enable = true;
      config = ''
        (pinentry-start)
      '';
    };

    # needed by Flycheck
    pkg-info = {
      enable = true;
      command = [ "pkg-info-version-info" ];
    };

    powerline = {
      enable = true;
      config = ''
        (powerline-center-evil-theme)
        (defadvice load-theme (after powerline-reset-after-load-theme activate)
          "Reset powerline after switching theme."
          (powerline-reset))
      '';
    };

    projectile = {
      enable = true;
      command = [ "projectile-mode" ];
      bindKeyMap."C-c p" = "projectile-command-map";
      config = ''
        (setq projectile-enable-caching t)
        (setq projectile-project-search-path '("~/src/"))
        (setq projectile-require-project-root nil)
        (projectile-mode 1)
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

    smartparens = {
      enable = true;
      diminish = [ "smartparens-mode" ];
    };

    solarized-theme = {
      enable = true;
      demand = true;
      config = ''
        (load-theme 'solarized-dark t)
      '';
    };

    subword = {
      enable = true;
      diminish = [ "subword-mode" ];
    };

    string-inflection = {
      enable = true;
      bind = { "C-c u" = "string-inflection-all-cycle"; };
    };

    undo-tree = {
      enable = true;
      diminish = [ "undo-tree-mode" ];
      config = "(global-undo-tree-mode)";
    };

    which-key = {
      enable = true;
      diminish = [ "which-key-mode" ];
      init = ''
        (setq which-key-separator " ")
        (setq which-key-prefix-prefix "+")
      '';
      config = "(which-key-mode 1)";
    };

    yaml-mode = {
      enable = true;
      mode = [ ''"\\.ya?ml\\'"'' ];
      hook = [ "(yaml-mode . (lambda () (mixed-pitch-mode -1)))" ];
    };

    yasnippet = {
      enable = true;
      diminish = [ "yas-minor-mode" ];
      config = ''
        (yas-global-mode 1)
      '';
    };

    yasnippet-snippets.enable = true;
  };
}
