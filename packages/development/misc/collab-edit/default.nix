{
  lib,
  stdenvNoCC,
  makeWrapper,
  bash,
  coreutils,
  gawk,
  gnugrep,
  gnused,
  git,
  jq,
  util-linux,
  shellcheck,
}:
stdenvNoCC.mkDerivation {
  pname = "collab-edit";
  version = "0.1.0";

  src = lib.fileset.toSource {
    root = ./.;
    fileset = lib.fileset.unions [
      ./collab-edit
      ./tests
    ];
  };

  nativeBuildInputs = [ makeWrapper ];
  buildInputs = [ bash ];
  # The tests use stubs in place of bud, tmux and emacsclient, and
  # throwaway repositories.
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
    shellcheck collab-edit tests/run.sh
    patchShebangs collab-edit tests/run.sh
    HOME=$TMPDIR bash tests/run.sh
    runHook postCheck
  '';

  # bud, tmux and emacsclient are left to the user's PATH: the command must
  # talk to the user's own sessions, tmux server and Emacs server, and does
  # without each of them.
  installPhase = ''
    runHook preInstall
    install -Dm755 collab-edit -t $out/bin
    wrapProgram $out/bin/collab-edit --prefix PATH : ${
      lib.makeBinPath [
        coreutils
        gawk
        git
        gnugrep
        gnused
        jq
        util-linux
      ]
    }
    runHook postInstall
  '';

  meta = {
    description = "Sessions in which an agent and a user edit one document together, for the collab-edit skill";
    license = lib.licenses.mit;
    platforms = lib.platforms.unix;
    mainProgram = "collab-edit";
  };
}
