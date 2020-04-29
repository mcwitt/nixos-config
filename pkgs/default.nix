{ callPackage }: {
  dotfiles = callPackage ./dotfiles.nix { };
  emacs = callPackage ./emacs.nix { };
  gitignore = callPackage ./gitignore.nix { };
  jupyterlab = callPackage ./jupyterlab { };
  scripts = callPackage ./scripts.nix { };
}
