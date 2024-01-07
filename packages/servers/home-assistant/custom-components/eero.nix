{ lib
, buildHomeAssistantComponent
, fetchFromGitHub
, pypng
, pyqrcode
}:

buildHomeAssistantComponent rec {
  owner = "schmittx";
  domain = "eero";
  version = "1.4.5";

  src = fetchFromGitHub {
    inherit owner;
    repo = "home-assistant-eero";
    rev = version;
    hash = "sha256-E09VUF2k0QdoP6+8q2F86dzuv+bRJyi+VbW925UmNyw=";
  };

  propagatedBuildInputs = [
    pypng
    pyqrcode
  ];

  dontCheckManifest = true; # pins pypng==0.0.20

  meta = with lib; {
    description = "Eero integration for Home Assistant";
    homepage = "https://github.com/schmittx/home-assistant-eero";
    license = licenses.mit;
    maintainers = with maintainers; [ ];
    mainProgram = "home-assistant-eero";
    platforms = platforms.all;
  };
}
