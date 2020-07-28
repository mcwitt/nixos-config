self: super:
let
  mypkgs = with { inherit (self) callPackage; }; {
    dotfiles = callPackage ./dotfiles.nix { };
    emacs = callPackage ./emacs.nix { };
    emacsPackages = callPackage ./emacs-packages.nix { };
    gitignore = callPackage ./gitignore.nix { };
    jupyterlab = callPackage ./jupyterlab { };
    myHaskellPackages = callPackage ./haskell-packages.nix { };
    scripts = callPackage ./scripts { };
    sources = callPackage ./sources { };
  };
in { inherit mypkgs; }
