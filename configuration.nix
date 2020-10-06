{ pkgs, ... }:

let sources = import ./nix/sources.nix;
in {
  imports = [ ./fonts.nix ./packages.nix ./cuda.nix ];

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

  programs.fish.enable = true;

  programs.gnupg.agent.enable = true;

  programs.tmux = {
    enable = true;
    keyMode = "vi";
  };

  hardware = {
    bluetooth.enable = true;
    opengl.driSupport32Bit = true;
    printers = rec {
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

    pulseaudio = {
      enable = true;
      package = pkgs.pulseaudioFull;
    };
  };

  i18n.defaultLocale = "en_US.UTF-8";

  location = {
    latitude = 37.77;
    longitude = 122.42;
  };

  networking = {
    hostName = "golem";
    interfaces.enp0s31f6.useDHCP = true;
  };

  nix.trustedUsers = [ "root" "@wheel" "matt" ];

  nixpkgs = {
    config.allowUnfree = true;
    overlays = (import ./utils.nix).importOverlaysDir ./overlays;
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

  # color temperature adjuster
  services.redshift.enable = true;

  services.xserver = {
    enable = true;
    videoDrivers = [ "nvidia" ];
    desktopManager.xterm.enable = true;
    xrandrHeads = [
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
  };

  sound = {
    enable = true;
    mediaKeys.enable = true;
  };

  system.stateVersion = "19.09"; # Did you read the comment?

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
