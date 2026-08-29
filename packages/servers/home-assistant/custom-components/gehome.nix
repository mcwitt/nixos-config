{
  lib,
  buildHomeAssistantComponent,
  fetchFromGitHub,
  gehomesdk,
  magicattr,
}:

buildHomeAssistantComponent rec {
  owner = "simbaja";
  domain = "ge_home";
  version = "2026.8.0";

  src = fetchFromGitHub {
    inherit owner;
    repo = "ha_gehome";
    rev = "v${version}";
    hash = "sha256-6PsZcLqa7csEGIg810arAiVpyiIUSgOgKcqqDhuhjIU=";
  };

  propagatedBuildInputs = [
    gehomesdk
    magicattr
  ];

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
