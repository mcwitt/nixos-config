self: super:
let
  emacsOverlay = (import (super.fetchFromGitHub {
    owner = "nix-community";
    repo = "emacs-overlay";
    rev = "4b89b7d5476ccf8ccdd31abb9d2d18267488e26a";
    sha256 = "1vni79i5pvkil12iafmf65q1chrkm6cv3xcy9kvpry59nrlxw2hx";
  }));

  emacsWithPackages = (self.emacsPackagesGen self.emacs).emacsWithPackages;

in {
  nixpkgs.overlays = [ emacsOverlay ];

  emacsEnv = emacsWithPackages (epkgs:
    with epkgs; [
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
      erc-hl-nicks
      erc-image
      evil
      evil-escape
      evil-magit
      evil-smartparens
      evil-surround
      exec-path-from-shell
      flycheck
      format-all
      general
      gist
      haskell-mode
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
      org-noter
      org-plus-contrib
      ox-gfm
      pdf-tools
      proof-general
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
    ]);
}
