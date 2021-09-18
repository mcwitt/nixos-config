{ config, lib, pkgs, ... }:
with lib;
let cfg = config.profiles.moonlander;
in
{
  options.profiles.moonlander.enable = mkEnableOption "Config for Moonlander keyboard";

  config = mkIf cfg.enable {
    hardware.keyboard.zsa.enable = true;
    environment.systemPackages = [ pkgs.wally-cli ];
  };
}
