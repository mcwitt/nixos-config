{ config, lib, pkgs, ... }: {

  imports = [
    ./evil.nix
    ./ivy.nix
    ./lsp.nix
    ./org.nix
    ./treemacs.nix
  ];

  programs.emacs.package = pkgs.emacsGcc;

  programs.emacs.init = {

    enable = true;
    recommendedGcSettings = true;

    earlyInit = ''
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

      (setq-default indent-tabs-mode nil)
      (show-paren-mode 1)

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
      bind = { "C-c b" = "browse-at-remote"; };
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

    direnv = {
      enable = true;
      command = [ "direnv-mode" "direnv-update-environment" ];
    };

    dockerfile-mode = {
      enable = true;
      mode = [ ''"Dockerfile\\'"'' ];
    };

    doom-themes = {
      enable = true;
      config = ''
        (setq doom-themes-enable-bold t)
        (setq doom-themes-enable-italic t)
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

    edit-indirect.enable = true;

    eldoc = {
      enable = true;
      diminish = [ "eldoc-mode" ];
      command = [ "eldoc-mode" ];
    };

    erc = {
      enable = true;
      command = [ "erc" ];
      bind = { "C-c e" = "erc-freenode"; };
      config = ''
        (setq erc-prompt-for-password nil) ; get login from ~/.authinfo.gpg
        (setq erc-hide-list '("JOIN" "PART" "QUIT"))

        (add-to-list 'erc-modules 'autojoin)
        (add-to-list 'erc-modules 'notifications)
        (add-to-list 'erc-modules 'spelling)
        (erc-update-modules)

        (setq erc-autojoin-channels-alist
              '(("#emacs"
                 "#haskell"
                 "#nixos"
                 "#org-mode"
                 "#python"
                 "freenode.net")))
        (setq erc-autojoin-timing 'ident)

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
        (global-flycheck-mode t)
      '';
    };

    format-all = {
      enable = true;
      diminish = [ "format-all-mode" ];
      hook = [ "(prog-mode . format-all-mode)" ];
    };

    forge = {
      enable = true;
      after = [ "magit" ];
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
      hook = [ "(json-mode . flycheck-mode)" ];
    };

    jupyter = {
      enable = false; # XXX Broken as of 2020-11-29
      bind = { "C-c j" = "jupyter-run-repl"; };
    };

    kubernetes = {
      enable = true;
      command = [ "kubernetes-overview" ];
    };

    logview.enable = true;

    password-store.enable = true;

    magit = {
      enable = true;
      bind = {
        "C-x g" = "magit-status";
        "C-x M-g" = "magit-dispatch";
      };
    };

    mixed-pitch = {
      enable = true;
      hook = [ "(text-mode . mixed-pitch-mode)" ];
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

    pinentry = {
      enable = true;
      config = "(pinentry-start)";
    };

    # needed by Flycheck
    pkg-info = {
      enable = true;
      command = [ "pkg-info-version-info" ];
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

    ripgrep = {
      enable = true;
      command = [ "ripgrep-regexp" ];
    };

    smartparens = {
      enable = true;
      diminish = [ "smartparens-mode" ];
    };

    subword = {
      enable = true;
      diminish = [ "subword-mode" ];
    };

    string-inflection = {
      enable = true;
      bind = { "C-c C-u" = "string-inflection-all-cycle"; };
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
      config = "(yas-global-mode 1)";
    };

    yasnippet-snippets.enable = true;
  };
}
