# https://nixos.wiki/wiki/NixOS_on_ARM#Installation
{ pkgs, ... }:
{
  imports = [
    ./duckdns.nix
    ./hardware-configuration.nix
    ./home-assistant
    ./zigbee2mqtt.nix
  ];

  boot.loader.grub.enable = false;
  boot.loader.generic-extlinux-compatible.enable = true;

  console.useXkbConfig = true;

  environment.systemPackages = [ pkgs.vim ];

  i18n.defaultLocale = "en_US.UTF-8";

  networking.hostName = "hestia";

  nix = {
    gc.automatic = true;
    gc.dates = "weekly";
    settings.trusted-public-keys = [ "golem:eibXP6qvkaDB9Jvh/MkR4D/dVL7HYDBJI2srJZgVhGE=" ];
  };

  services.home-assistant.enable = true;

  services.avahi = {
    enable = true;
    nssmdns = true;
    publish = {
      enable = true;
      addresses = true;
    };
  };

  services.openssh = {
    enable = true;
    settings.PasswordAuthentication = false;
  };

  system.stateVersion = "22.11";

  time.timeZone = "America/Los_Angeles";

  users.users.matt = {
    isNormalUser = true;
    extraGroups = [ "wheel" ];
  };
}
