{ pkgs, jupyter }:
jupyter.kernels.iHaskellWith {
  name = "haskell";
  packages = ps:
    with ps; [
      aeson
      array
      cassava
      containers
      fgl
      formatting
      heaps
      histogram-fill
      hmatrix
      hspec
      hvega
      ihaskell-hvega
      lens
      linear
      monad-bayes
      monad-loops
      mtl
      parsec
      postgresql-simple
      req
      singletons
      split
      statistics
      transformers
      vector
    ];
}
