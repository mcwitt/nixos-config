{ stdenv }:
stdenv.mkDerivation {
  name = "scripts";
  src = ./bin;
  dontBuild = true;
  installPhase = ''
    mkdir -p $out/bin
    find . -maxdepth 1 \( -type f -o -type l \) -executable \
      -exec cp -pL {} $out/bin \;
  '';
}
