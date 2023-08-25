{ pkgs, ... }:
{
  imports = [ ./sonos.nix ];

  services.home-assistant = {
    config = {
      default_config = { };
      mqtt = { };
      sonos = { };
      wake_on_lan = { };
    };

    extraComponents = [
      # Components required to complete the onboarding
      "esphome"
      "met"
      "radio_browser"

      # Integrations that do not support yaml configuration
      "brother"
      "cast"
      "flume"
      "google_translate" # text-to-speech provider
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
