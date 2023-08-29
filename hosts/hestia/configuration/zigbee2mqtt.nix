{ config, ... }:
{
  networking.firewall.allowedTCPPorts = [ config.services.zigbee2mqtt.settings.frontend.port ];

  services.mosquitto = {
    enable = true;

    listeners = [
      {
        acl = [ "pattern readwrite #" ];
        omitPasswordAuth = true;
        settings.allow_anonymous = true;
      }
    ];
  };

  services.zigbee2mqtt = {
    enable = true;

    settings = {
      homeassistant = config.services.home-assistant.enable;
      permit_join = true;
      advanced.network_key = [ 101 237 92 243 37 95 115 34 17 45 251 154 9 133 183 199 ];

      frontend.port = 8080;

      mqtt = {
        base_topic = "zigbee2mqtt";
        server = "mqtt://localhost:1883";
      };

      serial.port = "/dev/serial/by-id/usb-ITEAD_SONOFF_Zigbee_3.0_USB_Dongle_Plus_V2_20230602161357-if00";
    };
  };
}
