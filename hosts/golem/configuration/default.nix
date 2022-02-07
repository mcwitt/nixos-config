{ pkgs, ... }:
{
  imports = [
    <home-manager/nixos>
    ../../../modules/common/nixos
    ../../../modules/nixos/nixos
    ./synergy-server.nix
  ];

  boot.loader = {
    systemd-boot.enable = true;
    efi.canTouchEfiVariables = true;
  };

  console = {
    font = "${pkgs.terminus_font}/share/consolefonts/ter-u16n.psf.gz";
    keyMap = "us";
  };

  hardware.bluetooth = {
    enable = true;
    settings.General.ControllerMode = "bredr";
  };

  hardware.nvidia.modesetting.enable = true;

  hardware.opengl.driSupport32Bit = true;

  hardware.pulseaudio = {
    enable = true;
    package = pkgs.pulseaudioFull;
  };

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

  hardware.video.hidpi.enable = true;

  home-manager = {
    useGlobalPkgs = true;
    users.matt = {
      imports = [ ../home ];
      profiles.personal.enable = true;
    };
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

  nix.settings.trusted-users = [ "root" "@wheel" "matt" ];

  profiles = {
    personal.enable = true;
    moonlander.enable = true;
  };

  services.printing = {
    enable = true;
    drivers = [ pkgs.brlaser ];
  };

  services.xserver = {
    enable = true;
    videoDrivers = [ "nvidia" ];
    desktopManager.xterm.enable = true;
    layout = "us";
    dpi = 183;
    xrandrHeads = [
      {
        primary = true;
        output = "DP-4";
      }
      {
        output = "DP-2";
      }
    ];
  };

  sound = {
    enable = true;
    mediaKeys.enable = true;
  };

  system.stateVersion = "21.11";

  time.timeZone = "America/Los_Angeles";

  users.users.matt = {
    isNormalUser = true;
    extraGroups = [ "wheel" "docker" ];
    shell = pkgs.fish;
  };

  virtualisation.docker.enableNvidia = true;
}
