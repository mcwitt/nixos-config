{
  lib,
  buildNpmPackage,
  fetchFromGitHub,
}:

buildNpmPackage rec {
  pname = "scheduler-card";
  version = "4.0.14";

  src = fetchFromGitHub {
    owner = "nielsfaber";
    repo = "scheduler-card";
    rev = "refs/tags/v${version}";
    hash = "sha256-cW46bxD50p1kkCP729GsUDMO+iLkXJcil3lNgjrCsh0=";
  };

  postPatch = ''
    cp ${./package-lock.json} package-lock.json
  '';

  npmDepsHash = "sha256-MtqanT7yx94a2XzXxDxhAklPPsw/Wl1eeDVGckht31Q=";

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
