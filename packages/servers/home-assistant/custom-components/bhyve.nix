{
  lib,
  buildHomeAssistantComponent,
  fetchFromGitHub,
}:

buildHomeAssistantComponent rec {
  owner = "sebr";
  domain = "bhyve";
  version = "4.1.3";

  src = fetchFromGitHub {
    inherit owner;
    repo = "bhyve-home-assistant";
    rev = version;
    hash = "sha256-15JagJ6BsKh/wQsskal8xg5USFRtkor15k3KB2etQtk=";
  };

  postPatch = ''
    substituteInPlace custom_components/bhyve/__init__.py \
      --replace-fail 'return self.device_data.get("is_connected", False)' \
                     'return self.coordinator.last_update_success and bool(self.device_data)'
  '';

  meta = with lib; {
    description = "Orbit BHyve custom component for Home Assistant";
    homepage = "https://github.com/sebr/bhyve-home-assistant";
    license = licenses.mit;
    maintainers = with maintainers; [ ];
  };
}
