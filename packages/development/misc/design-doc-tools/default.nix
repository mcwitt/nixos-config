{
  lib,
  stdenvNoCC,
  makeWrapper,
  bash,
  gawk,
  shellcheck,
}:
stdenvNoCC.mkDerivation {
  pname = "design-doc-tools";
  version = "0.2.0";

  src = lib.fileset.toSource {
    root = ./.;
    fileset = lib.fileset.unions [
      ./design-doc-wc
      ./tests
    ];
  };

  nativeBuildInputs = [ makeWrapper ];
  buildInputs = [ bash ];
  nativeCheckInputs = [
    gawk
    shellcheck
  ];

  dontBuild = true;
  doCheck = true;
  checkPhase = ''
    runHook preCheck
    shellcheck design-doc-wc tests/run.sh
    patchShebangs design-doc-wc tests/run.sh
    bash tests/run.sh
    runHook postCheck
  '';

  installPhase = ''
    runHook preInstall
    install -Dm755 design-doc-wc -t $out/bin
    wrapProgram $out/bin/design-doc-wc --prefix PATH : ${lib.makeBinPath [ gawk ]}
    runHook postInstall
  '';

  meta = {
    description = "The word count for the design-doc-defaults skill";
    license = lib.licenses.mit;
    platforms = lib.platforms.unix;
  };
}
