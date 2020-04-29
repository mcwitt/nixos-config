{ pkgs, jupyter, ... }:
let
  haskellPackages = pkgs.haskell.packages.ghc865.extend (self: hspkgs: {
    monad-bayes = hspkgs.callPackage ../monad-bayes.nix { };
  });

in jupyter.kernels.iHaskellWith {
  name = "haskell";
  haskellPackages = haskellPackages;
  packages = ps:
    with ps; [
      aeson
      array
      cassava
      containers
      fgl
      formatting
      heaps
      hvega
      ihaskell-hvega
      lens
      monad-bayes
      monad-loops
      mtl
      parsec
      postgresql-simple
      req
      split
      statistics
      transformers
      vector
    ];
}
