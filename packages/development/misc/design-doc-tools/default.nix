{
  lib,
  stdenvNoCC,
  makeWrapper,
  bash,
  coreutils,
  gawk,
  gnused,
  git,
  jq,
  util-linux,
  shellcheck,
}:
stdenvNoCC.mkDerivation {
  pname = "design-doc-tools";
  version = "0.1.0";

  src = lib.fileset.toSource {
    root = ./.;
    fileset = lib.fileset.unions [
      ./design-doc-wc
      ./design-doc-signal
      ./tests
    ];
  };

  nativeBuildInputs = [ makeWrapper ];
  buildInputs = [ bash ];
  # The tests use a stub in place of bud and a throwaway repository.
  nativeCheckInputs = [
    gawk
    git
    jq
    shellcheck
    util-linux
  ];

  dontBuild = true;
  doCheck = true;
  checkPhase = ''
    runHook preCheck
    shellcheck design-doc-wc design-doc-signal tests/run.sh
    patchShebangs design-doc-wc design-doc-signal tests/run.sh
    HOME=$TMPDIR bash tests/run.sh
    runHook postCheck
  '';

  # bud is left to the user's PATH: design-doc-signal does nothing without
  # it, and the package would otherwise pull bud's own dependencies in.
  installPhase = ''
    runHook preInstall
    install -Dm755 design-doc-wc design-doc-signal -t $out/bin
    wrapProgram $out/bin/design-doc-wc --prefix PATH : ${lib.makeBinPath [ gawk ]}
    wrapProgram $out/bin/design-doc-signal --prefix PATH : ${
      lib.makeBinPath [
        coreutils
        git
        gnused
        jq
        util-linux
      ]
    }
    runHook postInstall
  '';

  meta = {
    description = "The word count and the commit signal for the design-doc-defaults skill";
    license = lib.licenses.mit;
    platforms = lib.platforms.unix;
  };
}
