{ pkgs, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
      ./fonts.nix
      ./packages.nix
      ./secret/wireless.nix
    ];

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  networking.hostName = "golem";
  networking.wireless.enable = true;

  # The global useDHCP flag is deprecated, therefore explicitly set to false here.
  # Per-interface useDHCP will be mandatory in the future, so this generated config
  # replicates the default behaviour.
  networking.useDHCP = false;
  networking.interfaces = {
    enp0s31f6.useDHCP = true;
    wlp4s0.useDHCP = true;
  };

  i18n.defaultLocale = "en_US.UTF-8";

  console = {
    font = "ter-u16n";
    keyMap = "us";
    packages = [ pkgs.terminus_font ];
  };

  time.timeZone = "America/Los_Angeles";

  location = {
    latitude = 37.77;
    longitude = 122.42;
  };

  fonts.fontconfig.dpi = 140;

  environment.systemPackages = with pkgs; [
    chromium
    dmenu
    firefox
    haskellPackages.xmobar
    signal-desktop
    termite
  ];

  programs.zsh.enable = true;
  programs.gnupg.agent.enable = true;

  programs.tmux = {
    enable = true;
    keyMode = "vi";
  };

  services.openssh.enable = true;
  services.redshift.enable = true;  # color temperature adjuster

  services.xserver = {
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
          { terminal = "termite"
          , modMask  = mod4Mask
          }
      '';
    };
    xrandrHeads = [
      { primary = true; output = "DP-4"; }
      { monitorConfig = ''
          Option "Rotate" "left"
        '';
        output = "DP-2";
      }
    ];
  };

  users.users.matt = {
    isNormalUser = true;
    extraGroups = [ "wheel" ]; # Enable ‘sudo’ for the user.
    shell = pkgs.zsh;
  };

  # This value determines the NixOS release with which your system is to be
  # compatible, in order to avoid breaking some software such as database
  # servers. You should change this only after NixOS release notes say you
  # should.
  system.stateVersion = "19.09"; # Did you read the comment?

  nixpkgs.config.allowUnfree = true;
}
