{ pkgs, ... }:
{
  imports = [
    <home-manager/nixos>
    ../../modules/common/nixos
    ../../modules/nixos/nixos
  ];

  # Use the systemd-boot EFI boot loader.
  boot.loader = {
    systemd-boot.enable = true;
    efi.canTouchEfiVariables = true;
  };

  console = {
    font = "ter-u16n";
    keyMap = "us";
    packages = [ pkgs.terminus_font ];
  };

  fonts.fontconfig.dpi = 140;

  libraries.cuda = {
    enable = true;
    package = pkgs.cudatoolkit_10_2;
  };

  programs.fish.enable = true;

  programs.gnupg.agent.enable = true;

  hardware.bluetooth.enable = true;

  hardware.enableAllFirmware = true;

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

  home-manager = {
    useGlobalPkgs = true;
    users.matt = { pkgs, ... }:
      let
        leftMonitorWidth = 3840;
        rightMonitorWidth = 2160;
        trayMaxIcons = 5;
        statusBarHeight = 36;
      in
      {
        imports = [
          ../../modules/common/home-manager
          ../../modules/nixos/home-manager
        ];
        profiles.personal.enable = true;

        programs.xmobar = {
          commands = [
            ''
              Run Weather "KSFO" [ "--template"
                                 , "<skyCondition> | <fc=#268bd2><tempF></fc>°F | <fc=#268bd2><rh></fc>% | <fc=#268bd2><pressure></fc>hPa"
                                 ] 36000
            ''
          ];
          config = {
            position = ''
              Static
                { xpos = 0
                , ypos = 0
                , width = ${toString (leftMonitorWidth - trayMaxIcons * statusBarHeight)}
                , height = ${toString statusBarHeight}
                }
            '';
            template = ''"%StdinReader% | %multicpu% | %coretemp% | %memory% | %dynnetwork% }{ %KSFO% | %date% |"'';
          };
        };

        services.gammastep = {
          latitude = 37.8044;
          longitude = -122.2712;
        };

        services.stalonetray.config = {
          geometry = "${toString trayMaxIcons}x1-${toString rightMonitorWidth}";
          icon_gravity = "NE";
          icon_size = statusBarHeight * 7 / 8;
          slot_size = statusBarHeight;
        };
      };
  };

  i18n.defaultLocale = "en_US.UTF-8";

  location = {
    latitude = 37.8044;
    longitude = -122.2712;
  };

  networking = {
    hostName = "golem";
    interfaces.enp0s31f6.useDHCP = true;
  };

  nix.trustedUsers = [ "root" "@wheel" "matt" ];

  nixpkgs = {
    config.allowUnfree = true;
    overlays = import ../../overlays;
  };

  services.avahi = {
    enable = true;
    nssmdns = true;
  };

  services.locate = {
    enable = true;
    interval = "hourly";
  };

  services.openssh.enable = true;

  services.printing = {
    enable = true;
    drivers = [ pkgs.brlaser ];
  };

  services.xserver.xrandrHeads = [
    {
      primary = true;
      output = "DP-4";
    }
    {
      monitorConfig = ''
        Option "Rotate" "left"
      '';
      output = "DP-2";
    }
  ];

  services.xserver = {
    enable = true;
    videoDrivers = [ "nvidia" ];
    desktopManager.xterm.enable = true;
  };

  sound = {
    enable = true;
    mediaKeys.enable = true;
  };

  system.stateVersion = "20.09";

  time.timeZone = "America/Los_Angeles";

  users.users.matt = {
    isNormalUser = true;
    extraGroups = [ "wheel" "docker" ];
    shell = pkgs.fish;
  };

  virtualisation.docker = {
    enable = true;
    enableNvidia = true;
  };
}
