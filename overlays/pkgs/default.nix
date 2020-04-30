_: super:
with super; {
  mypkgs = {
    dotfiles = callPackage ./dotfiles.nix { };
    emacs = callPackage ./emacs.nix { };
    gitignore = callPackage ./gitignore.nix { };
    jupyterlab = callPackage ./jupyterlab { };
    scripts = callPackage ./scripts.nix { };
  };
}
