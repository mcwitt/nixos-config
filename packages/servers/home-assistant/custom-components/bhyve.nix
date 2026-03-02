{
  lib,
  buildHomeAssistantComponent,
  fetchFromGitHub,
}:

buildHomeAssistantComponent rec {
  owner = "sebr";
  domain = "bhyve";
  version = "3.3.0";

  src = fetchFromGitHub {
    inherit owner;
    repo = "bhyve-home-assistant";
    rev = version;
    hash = "sha256-U+r+xJizd3j1jPMN3HWnBYRzErzBDKLojYmDlSIsyxs=";
  };

  meta = with lib; {
    description = "Orbit BHyve custom component for Home Assistant";
    homepage = "https://github.com/sebr/bhyve-home-assistant";
    license = licenses.mit;
    maintainers = with maintainers; [ ];
  };
}
