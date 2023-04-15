{ nerd-font-patcher, stdenv }:
font:
stdenv.mkDerivation {
  name = "${font.name}-nerd-font-patched";
  src = font;
  nativeBuildInputs = [ nerd-font-patcher ];
  buildPhase = ''
    mkdir build
    for f in share/fonts/truetype/*.ttf; do
        nerd-font-patcher -c --out build $f
    done
  '';
  installPhase = ''
    mkdir -p $out/share/fonts/truetype
    cp -a build/* $out/share/fonts/truetype/
  '';
}
