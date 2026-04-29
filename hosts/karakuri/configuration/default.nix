{
  config,
  inputs,
  lib,
  pkgs,
  ...
}:

{
  imports = [
    ./hardware-configuration.nix
    "${inputs.nixos-hardware}/lenovo/thinkpad/x1/7th-gen"
  ];

  boot.loader = {
    systemd-boot.enable = true;
    efi.canTouchEfiVariables = true;
  };

  console.keyMap = "us";

  hardware.acpilight.enable = true;

  hardware.bluetooth.enable = true;

  hardware.i2c.enable = true;
  users.groups.${config.hardware.i2c.group}.members = lib.attrNames (
    lib.filterAttrs (_: u: u.isNormalUser) config.users.users
  );
  environment.systemPackages = [ pkgs.ddcutil ];

  services.libinput = {
    enable = true;
    touchpad = {
      accelSpeed = "0.5";
      disableWhileTyping = true;
      naturalScrolling = true;
    };
  };

  networking = {
    hostName = "karakuri";
    networkmanager.enable = true;
    useDHCP = false;
    interfaces.wlp0s20f3.useDHCP = true;
  };

  services.autorandr = {
    enable = true;
    hooks.postswitch."restore-background" = ''
      ${pkgs.feh}/bin/feh --no-fehbg --bg-fill ${config.stylix.image}
    '';
  };

  services.blueman.enable = true;

  services.printing.enable = true;

  services.synergy.client = {
    enable = true;
    serverAddress = "satori";
  };

  stylix.fonts.sizes = lib.mkForce {
    applications = 10;
    desktop = 10;
    popups = 10;
    terminal = 10;
  };

  services.xserver = {
    enable = true;
    desktopManager.xterm.enable = true;
    displayManager.lightdm.greeters.gtk.extraConfig = ''
      xft-dpi=200
    '';
    xkb.layout = "us";
  };

  system.stateVersion = "21.11";

  time.timeZone = "America/Los_Angeles";
}
