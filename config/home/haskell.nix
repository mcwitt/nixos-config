{ pkgs, ... }:
let
  ghcide = import (pkgs.fetchFromGitHub {
    owner = "cachix";
    repo = "ghcide-nix";
    rev = "f940ec611cc6914693874ee5e024eba921cab19e";
    sha256 = "0vri0rivdzjvxrh6lzlwwkh8kzxsn82jp1c2w5rqzhp87y6g2k8z";
  }) { };

  haskellPackages = pkgs.haskell.packages.ghc883;

  ghcEnv = haskellPackages.ghcWithPackages (ps:
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
  home.packages = [
    ghcEnv

    # FIXME: As of 2020-03-20, latest compatible ghc for ghcide is
    # 8.6.5, but the 8.6 branch is unmaintained in nixpkgs and some
    # packages fail to build. Live with the mismatch for now.
    ghcide.ghcide-ghc865
  ] ++ (with haskellPackages; [ brittany hlint ]);
}
