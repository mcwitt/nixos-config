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
    };

    programs.gnupg.agent = {
      enable = true;
      pinentryPackage = pkgs.pinentry-gtk2;
      enableSSHSupport = true;
    };
  };
}
