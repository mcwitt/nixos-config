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
  home.packages = [ ghcEnv pkgs.ghcide.ghcide-ghc865 pkgs.stylish-cabal ]
    ++ (with pkgs.haskellPackages; [
      brittany
      cabal-install
      cabal2nix
      hlint
      ormolu
    ]);
}
