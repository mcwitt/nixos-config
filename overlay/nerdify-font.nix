{ nerd-font-patcher, stdenv }:
font:
stdenv.mkDerivation {
  name = "${font.name}-nerd-font-patched";
  src = font;
  nativeBuildInputs = [ nerd-font-patcher ];

  buildPhase = ''
    runHook preBuild

    mkdir build

    # Recursively find all .ttf files under share/fonts/truetype
    # -print0 and -0 handle filenames with spaces or special chars
    find share/fonts/truetype -name '*.ttf' -print0 | \
      xargs -P $NIX_BUILD_CORES -n 1 -0 nerd-font-patcher -c --out build

    runHook postBuild
  '';

  installPhase = ''
    runHook preInstall

    mkdir -p $out/share/fonts/truetype
    if [ -n "$(ls -A build)" ]; then
      cp -a build/* $out/share/fonts/truetype/
    fi

    runHook postInstall
  '';
}
