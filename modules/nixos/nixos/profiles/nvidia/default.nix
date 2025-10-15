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

    hardware.nvidia-container-toolkit = {
      enable = true;

      # https://github.com/NixOS/nixpkgs/issues/412324#issuecomment-3240508715
      extraArgs = [
        "--disable-hook"
        "create-symlinks"
      ];

      package = pkgs.nvidia-container-toolkit.overrideAttrs (old: {
        version = "git";
        src = pkgs.fetchFromGitHub {
          owner = "nvidia";
          repo = "nvidia-container-toolkit";
          rev = "08b3a388e7b1d447e10d4c4d4a71dca29a98a964"; # v1.18.0-rc.2
          hash = "sha256-y81UbNoMfIhl9Rf1H3RTRmGR3pysDtKlApLrIxwou9I=";
        };
        postPatch = ''
          substituteInPlace internal/config/config.go \
            --replace-fail '/usr/bin/nvidia-container-runtime-hook' "$tools/bin/nvidia-container-runtime-hook" \
            --replace-fail '/sbin/ldconfig' '${pkgs.glibc.bin}/sbin/ldconfig'

          # substituteInPlace tools/container/toolkit/toolkit.go \
          #   --replace-fail '/sbin/ldconfig' '${pkgs.glibc.bin}/sbin/ldconfig'

          substituteInPlace cmd/nvidia-cdi-hook/update-ldcache/update-ldcache.go \
            --replace-fail '/sbin/ldconfig' '${pkgs.glibc.bin}/sbin/ldconfig'
        '';
      });
    };

    services.xserver.videoDrivers = [ "nvidia" ];
  };
}
