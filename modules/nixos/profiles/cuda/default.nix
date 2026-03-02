{ config, lib, ... }:
{
  options.profiles.cuda.enable = lib.mkEnableOption "CUDA support with cuda-maintainers cache";

  config = lib.mkIf config.profiles.cuda.enable {
    nixpkgs.config.cudaSupport = true;

    nix.settings = {
      substituters = [ "https://cache.nixos-cuda.org" ];
      trusted-public-keys = [ "cache.nixos-cuda.org:74DUi4Ye579gUqzH4ziL9IyiJBlDpMRn9MBN8oNan9M=" ];
    };
  };
}
