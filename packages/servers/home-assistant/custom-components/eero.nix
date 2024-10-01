{ lib
, buildHomeAssistantComponent
, fetchFromGitHub
, pypng
, pyqrcode
}:

buildHomeAssistantComponent rec {
  owner = "schmittx";
  domain = "eero";
  version = "1.6.0";

  src = fetchFromGitHub {
    inherit owner;
    repo = "home-assistant-eero";
    rev = version;
    hash = "sha256-9yuvI7jSRfeTK1LMidws0BpYYN+ckgBs+bAUti3X/Sw=";
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
