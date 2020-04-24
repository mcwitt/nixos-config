{ pkgs, ... }:
let
  ghcide = import (pkgs.fetchFromGitHub {
    owner = "cachix";
    repo = "ghcide-nix";
    rev = "f940ec611cc6914693874ee5e024eba921cab19e";
    sha256 = "0vri0rivdzjvxrh6lzlwwkh8kzxsn82jp1c2w5rqzhp87y6g2k8z";
  }) { };

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
  home.packages = [ ghcEnv ghcide.ghcide-ghc865 ]
    ++ (with pkgs.haskellPackages; [
      brittany
      cabal-install
      cabal2nix
      hlint
      ormolu
    ]);
}
