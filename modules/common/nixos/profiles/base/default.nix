{
  config,
  lib,
  pkgs,
  ...
}:
{
  options.profiles.base.enable = lib.mkEnableOption "Base configuration enabled on most machines";

  config = lib.mkIf config.profiles.base.enable {

    fonts.enableDefaultPackages = true;

    environment.systemPackages = with pkgs; [
      (aspellWithDicts (ds: [ ds.en ]))
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
      rsync
      time
      tree
      vim
      watch
      wget
    ];

    nix.gc = {
      automatic = true;
      dates = "weekly";
    };

    nix.settings.auto-optimise-store = true;

    programs.gnupg.agent = {
      enable = true;
      pinentryPackage = pkgs.pinentry-gtk2;
      enableSSHSupport = true;
    };
  };
}
