{ pkgs, ... }:
{
  imports = [ ./sonos.nix ];

  services.home-assistant = {
    config = {
      default_config = { };
      homeassistant.auth_mfa_modules = [{ type = "totp"; }];
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
      "androidtv_remote"
      "brother"
      "cast"
      "flume"
      "google_translate" # text-to-speech provider
      "otp"
      "roomba"
      "speedtestdotnet"
    ];

    extraPackages = python3Packages: with python3Packages; [
      pyqrcode # needed for totp setup
    ];

    openFirewall = true;

    package = pkgs.home-assistant.override {
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
    };
  };
}
