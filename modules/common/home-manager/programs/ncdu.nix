{ config, lib, pkgs, ... }:
let cfg = config.programs.ncdu;
in
{
  options.programs.ncdu.enable = lib.mkEnableOption "Enable ncdu";

  config = lib.mkIf cfg.enable {
    home.packages = [ pkgs.ncdu ];
  };
}
