{
  services.home-assistant.config.sonos = { };

  # https://www.home-assistant.io/integrations/sonos/#network-requirements
  networking.firewall.allowedTCPPorts = [ 1400 ];
}
