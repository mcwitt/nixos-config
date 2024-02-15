{ config, inputs, lib, ... }:

let cfg = config.profiles.home-automation;
in
{
  imports = [ inputs.firefly.nixosModules.firefly-iii ];

  config = lib.mkIf cfg.enable {
    nixpkgs.overlays = [ inputs.firefly.overlays.default ];
    nix.settings = {
      substituters = [ "https://timhae-firefly.cachix.org" ];
      trusted-public-keys = [ "timhae-firefly.cachix.org-1:TMexYUvP5SKkeKG11WDbYUVLh/4dqvCqSE/c028sqis=" ];
    };
  };
}
