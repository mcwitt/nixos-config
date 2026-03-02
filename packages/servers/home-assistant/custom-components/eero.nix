{
  lib,
  buildHomeAssistantComponent,
  fetchFromGitHub,
  pypng,
  pyqrcode,
}:

buildHomeAssistantComponent rec {
  owner = "schmittx";
  domain = "eero";
  version = "1.8.1";

  src = fetchFromGitHub {
    inherit owner;
    repo = "home-assistant-eero";
    rev = version;
    hash = "sha256-iAY5ZlJaSZedykIOiRxULbrs+b8iK0pGed3fHbF3b8E=";
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
