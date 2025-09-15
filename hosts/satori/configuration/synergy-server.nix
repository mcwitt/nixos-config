{
  networking.firewall = {
    allowedTCPPorts = [ 24800 ];
    allowedUDPPorts = [ 24800 ];
  };

  services.synergy.server.enable = true;
}
