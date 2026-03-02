{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.profiles.nvidia;
in
{
  options.profiles.nvidia.enable = lib.mkEnableOption "Enable config for NVIDIA GPUs";
  config = lib.mkIf cfg.enable {

    # Enable access to performance counter to all users
    # https://developer.nvidia.com/nvidia-development-tools-solutions-err_nvgpuctrperm-permission-issue-performance-counters
    boot.extraModprobeConfig = ''
      options nvidia NVreg_RestrictProfilingToAdminUsers=0
    '';

    environment.systemPackages = [ pkgs.nvtopPackages.nvidia ];

    hardware.nvidia.modesetting.enable = true;

    hardware.nvidia-container-toolkit.enable = true;

    services.xserver.videoDrivers = [ "nvidia" ];
  };
}
