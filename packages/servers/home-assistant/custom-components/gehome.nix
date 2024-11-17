{ lib
, buildHomeAssistantComponent
, fetchFromGitHub
, gehomesdk
, magicattr
, slixmpp
}:

buildHomeAssistantComponent rec {
  owner = "simbaja";
  domain = "gehome";
  version = "0.6.12";

  src = fetchFromGitHub {
    inherit owner;
    repo = "ha_gehome";
    rev = "v${version}";
    hash = "sha256-gtZah44NlT1jnbxnxqbo0uWnxP+mWYkl9RfJNvLV/xU=";
  };

  propagatedBuildInputs = [ gehomesdk magicattr slixmpp ];

  dontCheckManifest = true; # pinned slixmpp version not in nixpkgs

  meta = with lib; {
    description = "GE Home Appliances (SmartHQ) for Home Assistant";
    homepage = "https://github.com/simbaja/ha_gehome";
    changelog = "https://github.com/simbaja/ha_gehome/blob/${src.rev}/CHANGELOG.md";
    license = licenses.mit;
    maintainers = with maintainers; [ ];
    mainProgram = "ha-gehome";
    platforms = platforms.all;
  };
}
