{
  lib,
  buildNpmPackage,
  fetchFromGitHub,
}:

buildNpmPackage rec {
  pname = "scheduler-card";
  version = "3.2.15";

  src = fetchFromGitHub {
    owner = "nielsfaber";
    repo = "scheduler-card";
    rev = "refs/tags/v${version}";
    hash = "sha256-9lPPvf4FPs3epWXM5mTymeE6GJ3jrL+pVBQeUTdLHlE=";
  };

  postPatch = ''
    cp ${./package-lock.json} package-lock.json
  '';

  npmDepsHash = "sha256-EA2wDm5u/oNk7hTKY4otqPtSj2FmY55QlZB5Lo5Urrg=";

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
