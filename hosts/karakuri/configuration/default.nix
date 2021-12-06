{ config, pkgs, ... }:

let sources = import ../../../nix/sources.nix;
in
{
  imports = [
    <home-manager/nixos>
    ../../../modules/common/nixos
    ../../../modules/nixos/nixos
    "${sources.nixos-hardware}/lenovo/thinkpad/x1/7th-gen"
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

  home-manager = {
    useGlobalPkgs = true;
    users.matt = {
      imports = [ ../home ];
      profiles.personal.enable = true;
    };
  };

  networking = {
    hostName = "karakuri";
    networkmanager.enable = true;
    useDHCP = false;
    interfaces.wlp0s20f3.useDHCP = true;
  };

  services.autorandr.enable = true;

  services.blueman.enable = true;

  services.geoclue2 = {
    enable = true;
    appConfig = {
      "gammastep" = {
        isAllowed = true;
        isSystem = false;
      };
    };
  };

  services.printing.enable = true;

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

  system.stateVersion = "20.09";

  time.timeZone = "America/Los_Angeles";

  users.users.matt = {
    isNormalUser = true;
    extraGroups = [ "docker" "video" "wheel" ];
    shell = pkgs.fish;
  };
}
