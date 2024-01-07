{ lib
, buildHomeAssistantComponent
, fetchFromGitHub
}:

buildHomeAssistantComponent rec {
  owner = "sebr";
  domain = "bhyve";
  version = "3.2.2";

  src = fetchFromGitHub {
    inherit owner;
    repo = "bhyve-home-assistant";
    rev = version;
    hash = "sha256-gJyQTTAogQ9M9A3J5kuX2G/fLy1mOfEplYcV6SLbH1E=";
  };

  meta = with lib; {
    description = "Orbit BHyve custom component for Home Assistant";
    homepage = "https://github.com/sebr/bhyve-home-assistant";
    license = licenses.mit;
    maintainers = with maintainers; [ ];
  };
}
