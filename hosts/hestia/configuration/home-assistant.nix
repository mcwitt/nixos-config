{ pkgs, ... }:
{
  imports = [ ./sonos.nix ];

  services.home-assistant = {
    config = {
      # Includes dependencies for a basic setup
      # https://www.home-assistant.io/integrations/default_config/
      default_config = { };

      homeassistant = {
        latitude = 37.8715;
        longitude = -122.2730;
        name = "Home";
      };

      mqtt = { };

      sonos = { };
    };

    extraComponents = [
      # Components required to complete the onboarding
      "esphome"
      "met"
      "radio_browser"

      "flume"
      "roomba"
    ];

    openFirewall = true;

    package = pkgs.home-assistant.override {
      packageOverrides = final: prev: {
        pyflume = prev.pyflume.overridePythonAttrs
          (oldAttrs: rec {
            version = "0.6.5"; # version pinned by home-assistant
            name = "${oldAttrs.pname}-${version}";
            src = pkgs.fetchFromGitHub {
              owner = "ChrisMandich";
              repo = "PyFlume";
              rev = "v${version}";
              hash = "sha256-kIE3y/qlsO9Y1MjEQcX0pfaBeIzCCHk4f1Xa215BBHo=";
            };
            nativeCheckInputs = oldAttrs.nativeCheckInputs ++ [ final.pytz ];
          });
      };
    };
  };
}
