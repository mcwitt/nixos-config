{ pkgs, ... }:

{
  imports = [
    ./hardware-configuration.nix
    ./fonts.nix
    ./packages.nix
    ./secret/wireless.nix
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

  environment.systemPackages = with pkgs; [
    chromium
    dmenu
    firefox
    haskellPackages.xmobar
    signal-desktop
  ];

  fonts.fontconfig.dpi = 140;

  programs = {
    zsh.enable = true;
    gnupg.agent.enable = true;

    tmux = {
      enable = true;
      keyMode = "vi";
    };
  };

  i18n.defaultLocale = "en_US.UTF-8";

  location = {
    latitude = 37.77;
    longitude = 122.42;
  };

  networking = {
    hostName = "golem";
    wireless.enable = true;

    # The global useDHCP flag is deprecated, therefore explicitly set to false here.
    # Per-interface useDHCP will be mandatory in the future, so this generated config
    # replicates the default behaviour.
    useDHCP = false;
    interfaces = {
      enp0s31f6.useDHCP = true;
      wlp4s0.useDHCP = true;
    };
  };

  nix.trustedUsers = [ "root" "@wheel" "matt" ];

  nixpkgs.config.allowUnfree = true;

  services = {
    emacs.enable = true;
    locate.enable = true;
    openssh.enable = true;
    redshift.enable = true; # color temperature adjuster

    xserver = {
      enable = true;
      videoDrivers = [ "nvidia" ];
      displayManager.defaultSession = "none+xmonad";
      windowManager.xmonad = {
        enable = true;
        enableContribAndExtras = true;
        config = ''
          import XMonad
          import XMonad.Hooks.DynamicLog

          main = xmonad =<< xmobar def
            { borderWidth        = 3
            , focusedBorderColor = "#859900"
            , modMask            = mod4Mask
            , terminal           = "urxvt"
            }
        '';
      };
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
  };

  # This value determines the NixOS release with which your system is to be
  # compatible, in order to avoid breaking some software such as database
  # servers. You should change this only after NixOS release notes say you
  # should.
  system.stateVersion = "19.09"; # Did you read the comment?

  time.timeZone = "America/Los_Angeles";

  users.users.matt = {
    isNormalUser = true;
    extraGroups = [ "wheel" ]; # Enable ‘sudo’ for the user.
    shell = pkgs.zsh;
  };
}
