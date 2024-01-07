{ lib
, buildHomeAssistantComponent
, fetchFromGitHub
}:

buildHomeAssistantComponent rec {
  owner = "nielsfaber";
  domain = "alarmo";
  version = "1.9.13";

  src = fetchFromGitHub {
    inherit owner;
    repo = "alarmo";
    rev = "v${version}";
    hash = "sha256-RDcJoe9Dyb9F4bOhmB/Yth/I2G3Oa1UQfOBLXWvKBxg=";
  };

  meta = with lib; {
    description = "Easy to use alarm system integration for Home Assistant";
    homepage = "https://github.com/nielsfaber/alarmo";
    license = licenses.unfree; # FIXME: nix-init did not found a license
    maintainers = with maintainers; [ ];
    mainProgram = "alarmo";
    platforms = platforms.all;
  };
}
