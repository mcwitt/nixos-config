{ config, lib, ... }:
{
  config = lib.mkIf config.profiles.home-automation.enable {

    networking.firewall.allowedTCPPorts = [ config.services.zigbee2mqtt.settings.frontend.port ];

    services.zigbee2mqtt = {
      enable = true;

      settings = {
        # Can be temporarily enabled via the frontend when adding devices
        permit_join = false;
        frontend.port = lib.mkDefault 8080;

        serial = {
          port = "/dev/serial/by-id/usb-ITEAD_SONOFF_Zigbee_3.0_USB_Dongle_Plus_V2_20230602161357-if00";
          adapter = "ember";
        };

        # https://www.zigbee2mqtt.io/guide/configuration/device-availability.html#availability-advanced-configuration
        availability = {
          active.timeout = 10;
          passive.timeout = 1500;
        };
      };
    };

    # Workaround to ensure restart with exit code 0 on "error: z2m: Adapter disconnected, stopping"
    systemd.services.zigbee2mqtt.serviceConfig.Restart = lib.mkForce "always";
  };
}
