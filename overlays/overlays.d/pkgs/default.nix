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
    scripts = callPackage ./scripts { };
  };
}
