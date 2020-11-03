{ pkgs, ... }: {
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

  nix.trustedUsers = [ "root" "@wheel" "matt" ];

  nixpkgs = {
    config.allowUnfree = true;
    overlays = import ./overlays;
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

  # color temperature adjuster
  services.redshift.enable = true;

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
