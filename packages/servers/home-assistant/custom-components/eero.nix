{ lib
, buildHomeAssistantComponent
, fetchFromGitHub
, pypng
, pyqrcode
}:

buildHomeAssistantComponent rec {
  owner = "schmittx";
  domain = "eero";
  version = "1.5.0";

  src = fetchFromGitHub {
    inherit owner;
    repo = "home-assistant-eero";
    rev = version;
    hash = "sha256-hOF/UH/qUoCoQssmOC3dqLkaoO4B7WKFZbqSnqLpV/Q=";
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
