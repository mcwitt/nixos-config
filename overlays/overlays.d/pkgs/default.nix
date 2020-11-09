self: super:
let
  inherit (self) callPackage;
  sources = import ../nix/sources.nix;
in
{
  mypkgs = {
    inherit sources;
    dotfiles = sources.mcwitt-dotfiles;
    emacs = callPackage ./emacs.nix { };
    emacsPackages = callPackage ./emacs-packages.nix { };
    gitignore = callPackage ./gitignore.nix { };
    jupyterlab = callPackage ./jupyterlab { };
    myHaskellPackages = callPackage ./haskell-packages.nix { };
    scripts = callPackage ./scripts { };
  };
}
