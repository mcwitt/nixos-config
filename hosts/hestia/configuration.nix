# https://nixos.wiki/wiki/NixOS_on_ARM#Installation
{ pkgs, ... }:
{
  imports = [
    ./hardware-configuration.nix
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
    settings.trusted-public-keys = [ "golem:ccFn2QC8Jpctrhlv6Z7SCXYJnvl1eJcvWpLb9tJ/Gck=" ];
  };

  services.openssh = {
    enable = true;
    settings.PermitRootLogin = "prohibit-password";
  };

  system.stateVersion = "22.11";

  time.timeZone = "America/Los_Angeles";

  users.users.matt = {
    isNormalUser = true;
    extraGroups = [ "wheel" ];
  };
}
