self: super: {
  haskellEnv = super.haskellPackages.ghcWithPackages (ps:
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
}
