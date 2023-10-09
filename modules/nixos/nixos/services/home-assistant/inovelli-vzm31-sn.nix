{ config, lib, pkgs, ... }:

with lib;

let
  cfg = config.services.home-assistant.inovelliVzm31sn;
  inherit (config.services.home-assistant) configDir;

in
{
  options.services.home-assistant.inovelliVzm31sn = {

    enable = mkEnableOption "Install blueprint exposing multi-tap actions for Inovelli VZM31-SN switch";

    path = mkOption {
      type = types.path;
      description = "Path to link blueprint yaml";
      default = "${configDir}/blueprints/automation/starbuck93/inovelli_vzm31-sn.yaml";
    };
  };

  config = mkIf cfg.enable {
    systemd.services.home-assistant.preStart =
      let
        src = pkgs.fetchurl {
          url = "https://gist.githubusercontent.com/starbuck93/650541c589193f50a1c0cf7ca903aea2/raw/be691813b3a1ea93543993dfb03f306a9d25b764/inovelli_vzm31-sn.yaml";
          hash = "sha256-v4U8CRRGz6WYdnM2/FP2CyqUzDKZDDU/AhWZE9T203Y=";
        };
      in
      ''
        mkdir -p ${builtins.dirOf cfg.path}
        ln -fns ${src} ${cfg.path}
      '';
  };
}
