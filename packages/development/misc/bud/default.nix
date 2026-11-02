{
  lib,
  rustPlatform,
  makeWrapper,
  tmux,
  git,
  worktrunk,
  fzf,
  util-linux,
}:
rustPlatform.buildRustPackage {
  pname = "bud";
  version = "0.1.0";

  src = lib.fileset.toSource {
    root = ./.;
    fileset = lib.fileset.unions [
      ./Cargo.toml
      ./Cargo.lock
      ./src
      ./tests
    ];
  };
  cargoLock.lockFile = ./Cargo.lock;

  nativeBuildInputs = [ makeWrapper ];
  # The integration tests run real tmux servers with stub agents; the stubs
  # use setsid to leave a process behind.
  nativeCheckInputs = [
    tmux
    git
    worktrunk
    util-linux
  ];

  postInstall = ''
    wrapProgram $out/bin/bud --prefix PATH : ${
      lib.makeBinPath [
        tmux
        git
        worktrunk
        fzf
      ]
    }
  '';

  meta = {
    description = "Launch agent sessions in tmux as a tree, and supervise them";
    license = lib.licenses.mit;
    mainProgram = "bud";
  };
}
