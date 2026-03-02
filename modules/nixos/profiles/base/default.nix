{
  config,
  inputs,
  lib,
  pkgs,
  ...
}:
{
  options.profiles.base.enable = lib.mkEnableOption "Base configuration enabled on most machines";

  config = lib.mkIf config.profiles.base.enable {

    fonts.enableDefaultPackages = true;

    i18n.defaultLocale = "en_US.UTF-8";

    environment.systemPackages = with pkgs; [
      (aspellWithDicts (
        ds: with ds; [
          en
          en-computers
          en-science
        ]
      ))
      coreutils
      dig
      file
      findutils
      gawk
      git
      gnugrep
      gnumake
      gnused
      gnutar
      gzip
      killall
      less
      lsof
      pstree
      rsync
      time
      tree
      unzip
      vim
      watch
      wget
    ];

    nix = {
      extraOptions = ''
        keep-outputs = true
        keep-derivations = true
        experimental-features = nix-command flakes
      '';

      gc = {
        automatic = true;
        persistent = true;
        dates = "03:00";
        options = "--delete-older-than 30d";
      };

      optimise = {
        automatic = true;
        dates = [ "03:30" ];
      };

      registry.nixpkgs.flake = inputs.nixpkgs;
    };

    programs.fish.enable = true;

    programs.gnupg.agent = {
      enable = true;
      pinentryPackage = pkgs.pinentry-gtk2;
      enableSSHSupport = true;
    };

    programs.mtr.enable = true;

    programs.nix-ld.enable = true;

    services.avahi = {
      enable = true;
      nssmdns4 = true;
    };

    services.earlyoom = {
      enable = true;
      enableNotifications = true;
    };

    services.hoogle = {
      enable = true;
      port = 8081;
      packages =
        ps: with ps; [
          aeson
          # array
          # containers
          lens
          lens-aeson
          monad-loops
          # mtl
          optparse-generic
          # parsec
          random-fu
          rvar
          safe
          split
          streaming
          # text
          turtle
          vector
        ];
    };

    services.locate = {
      enable = true;
      package = pkgs.mlocate;
      interval = "hourly";
    };

    services.openssh.enable = true;

    # needed for thunar to display thumbnail images
    services.tumbler.enable = true;

    services.udisks2.enable = true;

    virtualisation.podman = {
      enable = true;

      # Create an alias mapping docker to podman
      dockerCompat = true;

      # Required for containers under podman-compose to be able to talk to each other
      defaultNetwork.settings.dns_enabled = true;
    };
  };
}
