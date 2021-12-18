{ lib, ... }:
{
  networking.firewall = {
    allowedTCPPorts = lib.mkAfter [ 24800 ];
    allowedUDPPorts = lib.mkAfter [ 24800 ];
  };

  services.synergy.server.enable = true;
}
