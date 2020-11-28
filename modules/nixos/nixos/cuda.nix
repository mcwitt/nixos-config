{ config, lib, pkgs, ... }:
with lib;
let cfg = config.libraries.cuda;
in
{
  options.libraries.cuda = {
    enable = mkEnableOption "CUDA support";
    package = mkOption {
      type = types.package;
      default = pkgs.cudatoolkit;
    };
  };

  config = mkIf cfg.enable {
    environment.systemPackages = [ cfg.package ];

    hardware = {
      opengl.driSupport32Bit = true;
      nvidia.modesetting.enable = true;
    };

    nixpkgs.config.allowUnfree = true;
    services.xserver.videoDrivers = [ "nvidia" ];
  };
}
