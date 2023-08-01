{ inputs, pkgs, ... }: {

  nix.gc = {
    automatic = true;
    dates = "weekly";
  };

  fonts.enableDefaultPackages = true;

  environment.systemPackages = with pkgs; [
    (aspellWithDicts (ds: [ ds.en ]))
    coreutils
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

  services.geoclue2 = {
    enable = true;
    appConfig = {
      "gammastep" = {
        isAllowed = true;
        isSystem = false;
      };
    };
  };
}
