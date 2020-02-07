{ pkgs, ... }: {

  home.file.".emacs.d" = {
    source = "${pkgs.mcwitt-dotfiles}/emacs.d/";
    recursive = true;
  };

  programs.emacs.extraPackages = epkgs:
    with epkgs; [
      auctex
      browse-at-remote
      cdlatex
      company
      company-lsp
      company-restclient
      counsel
      counsel-projectile
      counsel-tramp
      deft
      dhall-mode
      direnv
      evil
      evil-escape
      evil-magit
      evil-smartparens
      evil-surround
      exec-path-from-shell
      flycheck
      general
      gist
      haskell-mode
      hl-todo
      intero
      ivy
      json-mode
      jupyter
      logview
      lsp-haskell
      lsp-ivy
      lsp-mode
      lsp-treemacs
      lsp-ui
      magit
      markdown-mode
      nix-mode
      nlinum-relative
      ob-restclient
      org-noter
      org-plus-contrib
      ox-gfm
      pdf-tools
      pinentry
      projectile
      restclient
      ripgrep
      smartparens
      sql-indent
      treemacs
      treemacs-evil
      treemacs-icons-dired
      treemacs-magit
      treemacs-projectile
      use-package
      which-key
      yaml-mode
      yasnippet
      yasnippet-snippets
      zenburn-theme
    ];
}
