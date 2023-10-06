{ config, ... }:
{
  networking.firewall.allowedTCPPorts = [ config.services.zigbee2mqtt.settings.frontend.port ];

  services.zigbee2mqtt = {
    enable = true;
    settings = {
      frontend.port = 8080;
      serial.port = "/dev/serial/by-id/usb-ITEAD_SONOFF_Zigbee_3.0_USB_Dongle_Plus_V2_20230602161357-if00";
    };
  };
}
