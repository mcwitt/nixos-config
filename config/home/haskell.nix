# Global Haskell environment for experimentation and ad-hoc usage
# (usually overridden by project-specific environment)
{ pkgs, ... }:
let
  ghcEnv = pkgs.haskellPackages.ghcWithPackages (ps:
    with ps; [
      array
      containers
      fgl
      heaps
      lens
      monad-loops
      mtl
      parsec
      split
      transformers
      vector
    ]);

in {
  home.packages = [ ghcEnv pkgs.stylish-cabal ] ++ (with pkgs.haskellPackages; [
    brittany
    cabal-install
    ghcid
    ghcide
    hlint
    ormolu
    stack
  ]);
}
