self: super: {
  scripts = with self;
    stdenv.mkDerivation {
      name = "scripts";
      src = ../bin;
      buildInputs = [ ];
      installPhase = ''
        mkdir -p $out/bin
        find . -maxdepth 1 \( -type f -o -type l \) -executable \
          -exec cp -pL {} $out/bin \;
      '';
    };
}
