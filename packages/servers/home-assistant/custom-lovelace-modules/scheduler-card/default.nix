{
  lib,
  buildNpmPackage,
  fetchFromGitHub,
}:

buildNpmPackage rec {
  pname = "scheduler-card";
  # Each release declares a minimum Home Assistant in its hacs.json; running a
  # card newer than our pinned HA renders the schedule popups scrambled
  # (nielsfaber/scheduler-card#1130). We run 2026.5.x: 4.0.18 wants >= 2026.5.0,
  # 4.0.19 wants >= 2026.6.0.
  # Newest compatible version until our pinned home-assistant catches up.
  version = "4.0.18";

  src = fetchFromGitHub {
    owner = "nielsfaber";
    repo = "scheduler-card";
    rev = "refs/tags/v${version}";
    hash = "sha256-hxoVds650qcwiwi/9n62A6/jS6AmuaIEssBOU6H8GHo=";
  };

  # package.json is upstream's plus one pin, with the lock regenerated against
  # it (verified to produce a dist byte-identical to upstream's committed
  # v4.0.18 artifact):
  #  - picomatch 2.3.1 added as a direct dependency so it hoists to the root
  #    and rollup-plugin-typescript2 dedupes to it: picomatch 2.3.2 breaks
  #    rpt2's file matching, so .ts files reach rollup untransformed (npm
  #    `overrides` would be cleaner but make `npm ci` hit the registry,
  #    which the sandboxed build forbids)
  # The typescript 5.8.3 exact pin carried for 4.0.10 is gone: the union-type
  # .find() that 5.9 rejected was fixed upstream, and this builds on 5.9.3.
  postPatch = ''
    cp ${./package.json} package.json
    cp ${./package-lock.json} package-lock.json
  '';

  npmDepsHash = "sha256-YKSqYGQfSYO31W2k+sgsBiEtt5gmgW53qnEjhaNE9d4=";

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
