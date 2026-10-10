{
  lib,
  buildNpmPackage,
  fetchFromGitHub,
}:

buildNpmPackage rec {
  pname = "scheduler-card";
  # Each release declares a minimum Home Assistant in its hacs.json; running a
  # card newer than HA renders the schedule popups scrambled
  # (nielsfaber/scheduler-card#1130). 4.0.19 requires HA >= 2026.6.0;
  # deploy it only alongside a compatible HA release.
  version = "4.0.19";

  src = fetchFromGitHub {
    owner = "nielsfaber";
    repo = "scheduler-card";
    rev = "refs/tags/v${version}";
    hash = "sha256-fHU5qhBbtSkEtHDQacgd6R1U+NV55VtPqfX8M56uUnw=";
  };

  # Use upstream's package.json and a generated package-lock.json for Nix's
  # npm dependency fetcher. The old direct picomatch 2.3.1 build pin is no
  # longer necessary with the retained lockfile; verify the compiled output
  # against upstream's dist on every update.
  postPatch = ''
    cp ${./package.json} package.json
    cp ${./package-lock.json} package-lock.json
  '';

  npmDepsHash = "sha256-1k6xz8z7yrzNA2KOpFfvwFsvbgkGhs8tZIEZiBMphwM=";

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
