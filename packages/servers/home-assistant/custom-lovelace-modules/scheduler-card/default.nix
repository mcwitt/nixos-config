{
  lib,
  buildNpmPackage,
  fetchFromGitHub,
}:

buildNpmPackage rec {
  pname = "scheduler-card";
  version = "4.0.18";

  src = fetchFromGitHub {
    owner = "nielsfaber";
    repo = "scheduler-card";
    rev = "refs/tags/v${version}";
    hash = "sha256-hxoVds650qcwiwi/9n62A6/jS6AmuaIEssBOU6H8GHo=";
  };

  postPatch = ''
    cp ${./package-lock.json} package-lock.json
  '';

  npmDepsHash = "sha256-z5NhNGpoV5I4BeUHH8oL/FylICVJraepqeKNRUIsAsQ=";

  # eslint and prettier are not in package.json dependencies;
  # skip lint/format and just run rollup
  npmBuildScript = "rollup";

  installPhase = ''
    runHook preInstall

    mkdir $out
    cp dist/scheduler-card.js $out

    runHook postInstall
  '';

  meta = with lib; {
    description = "HA Lovelace card for control of scheduler entities";
    homepage = "https://github.com/nielsfaber/scheduler-card";
    license = licenses.gpl3Only;
    maintainers = with maintainers; [ ];
    mainProgram = "scheduler-card";
    platforms = platforms.all;
  };
}
