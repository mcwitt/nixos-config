{
  lib,
  buildNpmPackage,
  fetchFromGitHub,
}:

buildNpmPackage rec {
  pname = "scheduler-card";
  # 4.0.11+ requires Home Assistant >= 2026.1 (see hacs.json upstream); we run
  # 2025.11.x, where the newer card renders the schedule popups scrambled.
  # Newest compatible version until our pinned home-assistant catches up.
  version = "4.0.10";

  src = fetchFromGitHub {
    owner = "nielsfaber";
    repo = "scheduler-card";
    rev = "refs/tags/v${version}";
    hash = "sha256-1WF32xxCYQCc6ettJsjQGGC0cH446jkFQ8c10tB41io=";
  };

  # package.json is upstream's plus two pins, with the lock regenerated
  # against it (verified to produce a dist byte-identical to upstream's
  # committed v4.0.10 artifact):
  #  - typescript 5.8.3 exact: 5.9 rejects a .find() on a union type in
  #    scheduler-combo-selector.ts (fixed upstream only after 4.0.10)
  #  - picomatch 2.3.1 added as a direct dependency so it hoists to the root
  #    and rollup-plugin-typescript2 dedupes to it: picomatch 2.3.2 breaks
  #    rpt2's file matching, so .ts files reach rollup untransformed (npm
  #    `overrides` would be cleaner but make `npm ci` hit the registry,
  #    which the sandboxed build forbids)
  postPatch = ''
    cp ${./package.json} package.json
    cp ${./package-lock.json} package-lock.json
  '';

  npmDepsHash = "sha256-pu2W1zJGUs4Y1gd5lBQpvNmDPrVNlQIh8EDRoDuY6RY=";

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
