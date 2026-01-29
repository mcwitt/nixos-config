{ config, lib, ... }:
{
  options.profiles.cuda.enable = lib.mkEnableOption "CUDA support with cuda-maintainers cache";

  config = lib.mkIf config.profiles.cuda.enable {
    nixpkgs.config.cudaSupport = true;

    nix.settings = {
      substituters = [ "https://cuda-maintainers.cachix.org" ];
      trusted-public-keys = [
        "cuda-maintainers.cachix.org-1:0dq3bujKpuEPMCX6U4WylrUDZ9JyUG0VpVZa7CNfq5E="
      ];
    };
  };
}
