{ pkgs, ... }:
let
  fetchEmacsOverlay =
    { owner ? "nix-community", repo ? "emacs-overlay", rev, sha256 }:
    builtins.fetchTarball {
      url = "https://github.com/${owner}/${repo}/archive/${rev}.tar.gz";
      sha256 = sha256;
    };
in {

  nixpkgs.overlays = [
    (import (fetchEmacsOverlay {
      rev = "4b89b7d5476ccf8ccdd31abb9d2d18267488e26a";
      sha256 = "1vni79i5pvkil12iafmf65q1chrkm6cv3xcy9kvpry59nrlxw2hx";
    }))
  ];

  home.file.".emacs.d" = {
    source = "${pkgs.mcwitt-dotfiles}/emacs.d/";
    recursive = true;
  };

  programs.emacs = {
    enable = true;

    extraPackages = epkgs:
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
      ];
  };
}
