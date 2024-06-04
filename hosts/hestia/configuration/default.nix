{ pkgs, ... }:
{
  imports = [ ./hardware-configuration.nix ];

  boot.loader.grub.enable = false;
  boot.loader.generic-extlinux-compatible.enable = true;

  console.useXkbConfig = true;

  environment.systemPackages = with pkgs; [
    home-assistant-cli
    vim
  ];

  i18n.defaultLocale = "en_US.UTF-8";

  networking.hostName = "hestia";

  nix = {
    gc.automatic = true;
    gc.dates = "weekly";
    settings.trusted-public-keys = [ "golem:eibXP6qvkaDB9Jvh/MkR4D/dVL7HYDBJI2srJZgVhGE=" ];
  };

  profiles.home-automation.enable = true;

  services.avahi = {
    enable = true;
    nssmdns4 = true;
    publish = {
      enable = true;
      addresses = true;
    };
  };

  services.openssh = {
    enable = true;
    settings.PasswordAuthentication = false;
  };

  services.zigbee2mqtt.settings.serial.port = "/dev/serial/by-id/usb-ITEAD_SONOFF_Zigbee_3.0_USB_Dongle_Plus_V2_20230602161357-if00";

  system.stateVersion = "22.11";

  time.timeZone = "America/Los_Angeles";

  users.users.matt = {
    isNormalUser = true;
    extraGroups = [ "wheel" ];
  };
}
