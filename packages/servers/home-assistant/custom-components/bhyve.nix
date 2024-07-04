{ lib
, buildHomeAssistantComponent
, fetchFromGitHub
}:

buildHomeAssistantComponent rec {
  owner = "sebr";
  domain = "bhyve";
  version = "3.2.4";

  src = fetchFromGitHub {
    inherit owner;
    repo = "bhyve-home-assistant";
    rev = version;
    hash = "sha256-g2lBUrABpKgwVJIU67mKmzfLJy2spb0w7WloIRJ8xsE=";
  };

  meta = with lib; {
    description = "Orbit BHyve custom component for Home Assistant";
    homepage = "https://github.com/sebr/bhyve-home-assistant";
    license = licenses.mit;
    maintainers = with maintainers; [ ];
  };
}
