{
  config,
  lib,
  pkgs,
  ...
}:

with lib;

let
  cfg = config.services.home-assistant.wakeUpLight;
  inherit (config.services.home-assistant) configDir;

in
{
  options.services.home-assistant.wakeUpLight = {

    enable = mkEnableOption "Install blueprint for sunrise wake-up light automation";

    path = mkOption {
      type = types.path;
      description = "Path to link blueprint yaml";
      default = "${configDir}/blueprints/automation/sbyx/wake-up-light-alarm-with-sunrise-effect.yaml";
    };
  };

  config = mkIf cfg.enable {
    systemd.services.home-assistant.preStart =
      let
        src = pkgs.fetchurl {
          url = "https://gist.githubusercontent.com/sbyx/96c43b13b90ae1c35b872313ba1d2d2d/raw/fc5dba10a35b882b74f42b6209d60e0084368212/wake-up-light-alarm-with-sunrise-effect.yaml";
          hash = "sha256-OtNoV20TlN2qP0FkpRTU7i7hoSHthY9BZZUhSnMq+W8=";
        };
      in
      ''
        mkdir -p ${builtins.dirOf cfg.path}
        ln -fns ${src} ${cfg.path}
      '';
  };
}
