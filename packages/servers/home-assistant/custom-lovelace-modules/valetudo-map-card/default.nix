{ lib
, buildNpmPackage
, fetchFromGitHub
}:

buildNpmPackage rec {
  pname = "valetudo-map-card";
  version = "2023.04.0";

  src = fetchFromGitHub {
    owner = "Hypfer";
    repo = "lovelace-valetudo-map-card";
    rev = "refs/tags/v${version}";
    hash = "sha256-owOIbA1tRlnbWJ/p/wAUpeDnz/Wzu+GmUammJ6VFxHc=";
  };

  patches = [ ./0001-Remove-git-dependency.patch ];

  npmDepsHash = "sha256-0gUEIi1ZhfvRcL20jalPOMlWqeo4nmWM8Czf1/Z6fNY=";

  installPhase = ''
    runHook preInstall

    mkdir $out
    cp dist/valetudo-map-card.js $out

    runHook postInstall
  '';

  meta = with lib; {
    description = "Display the map from a valetudo-enabled robot in a home assistant dashboard card.";
    homepage = "https://github.com/Hypfer/lovelace-valetudo-map-card";
    license = licenses.mit;
    maintainers = with maintainers; [ ];
    platforms = platforms.all;
  };
}
