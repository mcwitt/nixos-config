{
  lib,
  buildHomeAssistantComponent,
  fetchFromGitHub,
}:

buildHomeAssistantComponent rec {
  owner = "sebr";
  domain = "bhyve";
  version = "3.2.5";

  src = fetchFromGitHub {
    inherit owner;
    repo = "bhyve-home-assistant";
    rev = version;
    hash = "sha256-7FKZqGVskK8fI48BPOQGg4BpvC0zdVIjbIoKAk/KFIk=";
  };

  meta = with lib; {
    description = "Orbit BHyve custom component for Home Assistant";
    homepage = "https://github.com/sebr/bhyve-home-assistant";
    license = licenses.mit;
    maintainers = with maintainers; [ ];
  };
}
