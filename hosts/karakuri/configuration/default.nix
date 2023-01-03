{ inputs, pkgs, ... }:

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

  hardware.pulseaudio = {
    enable = true;
    package = pkgs.pulseaudioFull;
  };

  hardware.video.hidpi.enable = true;

  networking = {
    hostName = "karakuri";
    networkmanager.enable = true;
    useDHCP = false;
    interfaces.wlp0s20f3.useDHCP = true;
  };

  profiles.personal.enable = true;

  services.autorandr.enable = true;

  services.blueman.enable = true;

  services.printing.enable = true;

  services.synergy.client = {
    enable = true;
    serverAddress = "golem";
  };

  services.xserver = {
    enable = true;
    desktopManager.xterm.enable = true;
    layout = "us";
    dpi = 240;
    libinput = {
      enable = true;
      touchpad = {
        accelSpeed = "0.5";
        disableWhileTyping = true;
        naturalScrolling = true;
      };
    };
  };

  sound = {
    enable = true;
    mediaKeys.enable = true;
  };

  system.stateVersion = "21.11";

  time.timeZone = "America/Los_Angeles";
}
