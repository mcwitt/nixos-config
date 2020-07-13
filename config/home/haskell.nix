# Global Haskell environment for experimentation and ad-hoc usage
# (usually overridden by project-specific environment)
{ pkgs, ... }:
let
  ghcEnv = pkgs.haskellPackages.ghcWithPackages (ps:
    with ps; [
      array
      checkers
      containers
      fgl
      heaps
      hspec
      lens
      monad-loops
      mtl
      parsec
      split
      transformers
      vector
    ]);

in {
  home.packages = [ ghcEnv ] ++ (with pkgs.haskellPackages; [
    brittany
    cabal-fmt
    cabal-install
    ghcid
    ghcide
    hlint
    ormolu
    stack
  ]);
}
