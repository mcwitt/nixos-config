{ pkgs, ... }:
{
  imports = [
    ./hardware-configuration.nix
    ./android.nix
    ./arduino.nix
    ./synergy-server.nix
  ];

  boot.loader = {
    systemd-boot.enable = true;
    efi.canTouchEfiVariables = true;
  };

  console.keyMap = "us";

  hardware.bluetooth.enable = true;

  hardware.keyboard.zsa.enable = true;

  hardware.nvidia.modesetting.enable = true;
  hardware.nvidia.open = false;

  hardware.nvidia-container-toolkit.enable = true;

  hardware.graphics.enable = true;
  hardware.graphics.enable32Bit = true;

  hardware.printers = rec {
    ensureDefaultPrinter = "Brother_HL-L2340D_series";
    ensurePrinters = [{
      deviceUri =
        "dnssd://Brother%20HL-L2340D%20series._ipp._tcp.local/?uuid=e3248000-80ce-11db-8000-40490f90f0a2";
      model = "drv:///brlaser.drv/brl2340d.ppd";
      name = ensureDefaultPrinter;
      ppdOptions = {
        Duplex = "DuplexNoTumble";
        PageSize = "A4";
      };
    }];
  };

  location = {
    latitude = 37.8044;
    longitude = -122.2712;
  };

  networking = {
    hostName = "golem";
    useDHCP = false;
    interfaces.enp0s31f6.useDHCP = true;
  };

  nix.settings = {
    trusted-users = [ "root" "@wheel" ];
    secret-key-files = [ "/etc/nix/secret-key" ];
  };


  services.printing = {
    enable = true;
    drivers = [ pkgs.brlaser ];
  };

  services.xserver = {
    enable = true;
    videoDrivers = [ "nvidia" ];
    desktopManager.xterm.enable = true;
    xkb.layout = "us";
    xrandrHeads = [
      {
        primary = true;
        output = "DP-4";
      }
      {
        monitorConfig = ''Option "Rotate" "left"'';
        output = "DP-2";
      }
    ];
  };

  system.stateVersion = "21.11";

  time.timeZone = "America/Los_Angeles";
}
