{ config, lib, pkgs, ... }:

with lib;

let cfg = config.profiles.home-automation;
in

{
  imports = [
    ./eero.nix
    ./postgresql.nix
    ./solar.nix
  ];

  options.profiles.home-automation.enable = mkEnableOption "Services for home automation";

  config = mkIf cfg.enable {

    services.home-assistant = {
      enable = true;

      config = {
        default_config = { };

        # Allow adding automations via the UI
        # Note: automations added in config must be in labeled blocks
        # See https://www.home-assistant.io/docs/automation/yaml/
        automation = "!include automations.yaml";

        homeassistant.auth_mfa_modules = [{ type = "totp"; }];

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

      inovelliVzm31sn.enable = true;
      wakeUpLight.enable = true;

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
        "opower"
        "otp"
        "purpleair"
        "roomba"
        "speedtestdotnet"
        "zha"
      ];

      extraPackages = python3Packages: with python3Packages; [
        pyqrcode # needed for totp setup
      ];

      openFirewall = true;

      package = (pkgs.home-assistant.override {

        extraPackages = ps: with ps; [
          psycopg2 # TODO: move to postgresql module
          pypng # TODO: move to eero module
        ];

        packageOverrides = final: prev: with final; {
          androidtvremote2 = buildPythonPackage rec {
            pname = "androidtvremote2";
            version = "0.0.14";
            format = "pyproject";
            src = pkgs.fetchFromGitHub {
              owner = "tronikos";
              repo = "androidtvremote2";
              rev = "refs/tags/v${version}";
              hash = "sha256-m53TlNrrCjA4CqvR02Yph7Gr5Dt17VJFBX6MC3arWOI=";
            };
            nativeBuildInputs = [
              setuptools
            ];
            propagatedBuildInputs = [
              aiofiles
              cryptography
              protobuf
            ];
          };
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
              nativeCheckInputs = oldAttrs.nativeCheckInputs ++ [ pytz ];
            });
        };
      }).overrideAttrs (_: { doInstallCheck = false; });
    };

    services.paperless.enable = true;

    services.zigbee2mqtt.enable = true;
  };
}
