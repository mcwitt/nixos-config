{ inputs, pkgs, ... }:
{
  imports = [
    inputs.disko.nixosModules.disko
    ./android.nix
    ./arduino.nix
    ./cuda.nix
    ./synergy-server.nix
  ];

  # https://github.com/nix-community/disko?tab=readme-ov-file#sample-configuration-and-cli-command
  disko.devices.disk.primary = {
    type = "disk";
    device = "/dev/nvme0n1";
    content = {
      type = "gpt";
      partitions = {
        ESP = {
          type = "EF00";
          size = "512M";
          content = {
            type = "filesystem";
            format = "vfat";
            mountpoint = "/boot";
            mountOptions = [ "umask=0077" ];
          };
        };
        swap = {
          size = "16G";
          content = {
            type = "swap";
          };
        };
        root = {
          size = "100%";
          content = {
            type = "filesystem";
            format = "ext4";
            mountpoint = "/";
            mountOptions = [ "noatime" ];
          };
        };
      };
    };
  };

  boot.loader = {
    systemd-boot.enable = true;
    efi.canTouchEfiVariables = true;
  };

  console.keyMap = "us";

  environment.systemPackages = [ pkgs.nvtopPackages.nvidia ];

  hardware.bluetooth.enable = true;

  hardware.keyboard.zsa.enable = true;

  hardware.nvidia.modesetting.enable = true;
  hardware.nvidia.open = false;

  hardware.nvidia-container-toolkit.enable = true;

  hardware.graphics.enable = true;
  hardware.graphics.enable32Bit = true;

  hardware.printers = rec {
    ensureDefaultPrinter = "Brother_HL-L2340D_series";
    ensurePrinters = [
      {
        deviceUri = "dnssd://Brother%20HL-L2340D%20series._ipp._tcp.local/?uuid=e3248000-80ce-11db-8000-40490f90f0a2";
        model = "drv:///brlaser.drv/brl2340d.ppd";
        name = ensureDefaultPrinter;
        ppdOptions = {
          Duplex = "DuplexNoTumble";
          PageSize = "A4";
        };
      }
    ];
  };

  location = {
    latitude = 37.8044;
    longitude = -122.2712;
  };

  networking.hostName = "satori";

  nix.settings.trusted-users = [
    "root"
    "@wheel"
  ];

  services.printing = {
    enable = true;
    drivers = [ pkgs.brlaser ];
  };

  services.xserver = {
    enable = true;
    videoDrivers = [ "nvidia" ];
    xkb.layout = "us";
    xrandrHeads = [
      {
        primary = true;
        output = "DP-2";
      }
      {
        monitorConfig = ''Option "Rotate" "left"'';
        output = "DP-0";
      }
    ];
  };

  system.stateVersion = "25.05";

  time.timeZone = "America/Los_Angeles";
}
