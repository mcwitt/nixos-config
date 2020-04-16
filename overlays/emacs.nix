self: super:
let emacsWithPackages = (self.emacsPackagesGen self.emacs).emacsWithPackages;
in {
  emacsEnv = emacsWithPackages (epkgs:
    with epkgs; [
      all-the-icons
      anki-editor
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
      doom-themes
      erc-hl-nicks
      erc-image
      ess
      esup
      evil
      evil-escape
      evil-magit
      evil-smartparens
      evil-surround
      exec-path-from-shell
      fira-code-mode
      flycheck
      format-all
      general
      gist
      haskell-mode
      haskell-snippets
      hl-todo
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
      org-evil
      org-noter
      org-plus-contrib
      ox-gfm
      pdf-tools
      pinentry
      projectile
      proof-general
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
    ]);
}
