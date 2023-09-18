{ config, ... }:
{
  networking.firewall.allowedTCPPorts = [ config.services.zigbee2mqtt.settings.frontend.port ];

  services.zigbee2mqtt = {
    enable = true;

    settings = {
      homeassistant = config.services.home-assistant.enable;
      permit_join = false;
      frontend.port = 8080;

      mqtt = {
        base_topic = "zigbee2mqtt";
        server = "mqtt://localhost:1883";
      };

      serial.port = "/dev/serial/by-id/usb-ITEAD_SONOFF_Zigbee_3.0_USB_Dongle_Plus_V2_20230602161357-if00";
    };
  };
}
