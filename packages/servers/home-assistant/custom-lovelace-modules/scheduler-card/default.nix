{
  lib,
  buildNpmPackage,
  fetchFromGitHub,
}:

buildNpmPackage rec {
  pname = "scheduler-card";
  version = "3.2.13";

  src = fetchFromGitHub {
    owner = "nielsfaber";
    repo = "scheduler-card";
    rev = "refs/tags/v${version}";
    hash = "sha256-kfCc12InvPlV18JZimHDF/sXkVpjHvWGKZF1EHdZA7M=";
  };

  postPatch = ''
    cp ${./package-lock.json} package-lock.json
  '';

  npmDepsHash = "sha256-WcBYKHsbWlY0ICtgfZuI4qMjfeqgdCQNFpxXpuRFdbQ=";

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
