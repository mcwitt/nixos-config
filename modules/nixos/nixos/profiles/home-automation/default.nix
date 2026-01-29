{
  config,
  lib,
  pkgs,
  ...
}:

with lib;

let
  cfg = config.profiles.home-automation;
in

{
  imports = [
    ./lifx.nix
    ./recorder.nix
    ./solar.nix
    ./zigbee2mqtt.nix
  ];

  options.profiles.home-automation.enable = mkEnableOption "Services for home automation";

  config = mkIf cfg.enable {

    # Allow Home Assistant service user to shutdown/reboot without sudo
    security.polkit.extraConfig = ''
      polkit.addRule(function(action, subject) {
        if (action.id == "org.freedesktop.login1.power-off" ||
            action.id == "org.freedesktop.login1.reboot") {
          if (subject.user == "hass") {
            return polkit.Result.YES;
          }
        }
      });
    '';

    services.home-assistant = {
      enable = true;

      config = {
        default_config = { };

        # Allow adding automations via the UI
        # Note: automations added in config must be in labeled blocks
        # See https://www.home-assistant.io/docs/automation/yaml/
        automation = "!include automations.yaml";

        # Allow adding scenes via the UI
        scene = "!include scenes.yaml";

        homeassistant.auth_mfa_modules = [ { type = "totp"; } ];

        mqtt = { };

        sensor = [
          {
            platform = "time_date";
            display_options = [
              "date"
              "date_time_iso"
            ];
          }
        ];

        thread = { };
        wake_on_lan = { };
      };

      customComponents = with pkgs.home-assistant-custom-components; [
        bhyve
        eero
        gehome
        scheduler-component
      ];

      customLovelaceModules = with pkgs.home-assistant-custom-lovelace-modules; [
        scheduler-card
        valetudo-map-card
      ];

      extraComponents = [
        # Components required to complete the onboarding
        "esphome"
        "met"
        "radio_browser"

        # Integrations that do not support yaml configuration
        "androidtv_remote"
        "brother"
        "cast"
        "flume"
        "google_translate" # text-to-speech provider
        "ipp"
        "local_todo"
        "opower"
        "otp"
        "purpleair"
        "roomba"
        "speedtestdotnet"
        "wled"
        "zha"
        "zwave_js"
      ];

      extraPackages =
        ps: with ps; [
          pyqrcode # needed for totp setup
        ];

      openFirewall = true;

      package =
        (pkgs.home-assistant.override {
          packageOverrides = final: prev: {
            pyflume = prev.pyflume.overridePythonAttrs (oldAttrs: rec {
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
        }).overrideAttrs
          (_: {
            doInstallCheck = false;
          });
    };

    systemd.services.home-assistant.serviceConfig.Restart = lib.mkForce "always";
  };
}
