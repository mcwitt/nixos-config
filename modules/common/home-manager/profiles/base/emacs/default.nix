{ config, lib, pkgs, ... }: {
  imports = [
    ./evil.nix
    ./ivy.nix
    ./lsp.nix
    ./org.nix
    ./treemacs.nix
  ];
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
    '';

    usePackage = {

      all-the-icons.enable = true;

      beacon = {
        enable = true;
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
        command = [ "company-mode" "company-doc-buffer" "global-company-mode" ];
        bindLocal.company-active-map.jk = "company-complete";
        config = "(global-company-mode)";
      };

      company-restclient = {
        enable = true;
        after = [ "company" "restclient" ];
      };

      direnv = {
        enable = true;
        command = [ "direnv-mode" "direnv-update-environment" ];
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

      esup = {
        enable = true;
        command = [ "esup" ];
      };

      fira-code-mode.enable = true;

      flycheck = {
        enable = true;
        init = ''(setq ispell-program-name "aspell")'';
        config = "(global-flycheck-mode t)";
      };

      format-all = {
        enable = true;
        bind."C-c C-f" = "format-all-buffer";
      };

      gist.enable = true;

      hl-todo = {
        enable = true;
        hook = [ "(prog-mode . hl-todo-mode)" ];
      };

      imenu = {
        enable = true;
        bind."C-c i" = "imenu";
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

      latex = {
        enable = true;
        package = epkgs: epkgs.auctex;
        mode = [ ''("\\.tex\\'" . latex-mode)'' ];
        config = ''
          (add-to-list 'TeX-view-program-selection '(output-pdf "PDF Tools"))'';
      };

      logview.enable = true;

      magit.enable = true;

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
        bindKeyMap."C-c p" = "projectile-command-map";
        config = ''
          (setq projectile-require-project-root nil)
          (setq projectile-project-search-path '("~/src/"))
          (projectile-mode 1)
        '';
      };

      rainbow-delimiters = {
        enable = true;
        hook = [ "(prog-mode . rainbow-delimiters-mode)" ];
      };

      restclient.enable = true;

      ripgrep = {
        enable = true;
        command = [ "ripgrep-regexp" ];
      };

      smartparens.enable = true;

      undo-tree = {
        enable = true;
        config = "(global-undo-tree-mode)";
      };

      which-key = {
        enable = true;
        init = ''
          (setq which-key-separator " ")
          (setq which-key-prefix-prefix "+")
        '';
        config = "(which-key-mode 1)";
      };

      yaml-mode.enable = true;

      yasnippet = {
        enable = true;
        config = "(yas-global-mode 1)";
      };

      yasnippet-snippets.enable = true;
    };
  };
}
