{
  services.hoogle = {
    enable = true;
    port = 8081;
    packages = ps: with ps; [
      aeson
      # array
      # containers
      lens
      lens-aeson
      monad-loops
      # mtl
      optparse-generic
      # parsec
      random-fu
      rvar
      safe
      split
      streaming
      # text
      turtle
      vector
    ];
  };
}
