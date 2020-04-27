{ callPackage }: {
  dotfiles = callPackage ./dotfiles.nix { };
  emacs = callPackage ./emacs.nix { };
  gitignore = callPackage ./gitignore.nix { };
  jupyterlab = callPackage ./jupyterlab.nix { };
  scripts = callPackage ./scripts.nix { };
}
