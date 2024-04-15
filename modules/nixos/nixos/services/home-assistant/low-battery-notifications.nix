{ config, lib, pkgs, ... }:

with lib;

let
  cfg = config.services.home-assistant.lowBatteryNotifications;
  inherit (config.services.home-assistant) configDir;

in
{
  options.services.home-assistant.lowBatteryNotifications = {

    enable = mkEnableOption "Install blueprint for low battery notifications";

    path = mkOption {
      type = types.path;
      description = "Path to link blueprint yaml";
      default = "${configDir}/blueprints/automation/sbyx/low-battery-level-detection-notification-for-all-battery-sensors.yaml";
    };
  };

  config = mkIf cfg.enable {
    systemd.services.home-assistant.preStart =
      let
        src = pkgs.fetchurl {
          url = "https://gist.githubusercontent.com/sbyx/1f6f434f0903b872b84c4302637d0890/raw/89b5bbe49bbc9fc71271434bcf71fd0f03399235/low-battery-level-detection-notification-for-all-battery-sensors.yaml";
          hash = "sha256-yavhVwLKwaCnwTiHOEGkpOi0PyLyZ6/XXwFLKwN3Cfo=";
        };
      in
      ''
        mkdir -p ${builtins.dirOf cfg.path}
        ln -fns ${src} ${cfg.path}
      '';
  };
}
