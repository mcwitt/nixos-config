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
  version = "2026.6.0";

  src = fetchFromGitHub {
    inherit owner;
    repo = "ha_gehome";
    rev = "v${version}";
    hash = "sha256-WVDTq6oTA+ep9W2sp8czTqlkucwKr9ma7thaz9Q64EY=";
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
