{ config, lib, ... }:
{
  config = lib.mkIf config.services.zigbee2mqtt.enable {

    networking.firewall.allowedTCPPorts = [ config.services.zigbee2mqtt.settings.frontend.port ];

    services.zigbee2mqtt.settings.frontend.port = lib.mkDefault 8080;
  };
}
