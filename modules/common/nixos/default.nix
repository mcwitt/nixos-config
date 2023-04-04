{ inputs, pkgs, ... }: {

  nix.gc = {
    automatic = true;
    dates = "weekly";
  };

  nixpkgs.config = {
    allowUnfree = true;
    joypixels.acceptLicense = true;
  };

  fonts = {
    enableDefaultFonts = true;
    fonts = with pkgs; [
      emacs-all-the-icons-fonts
      fira-code-symbols
      iosevka
      iosevka-comfy.comfy
      joypixels
      (nerdfonts.override { fonts = [ "FiraCode" "Iosevka" ]; })
    ];
  };

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
