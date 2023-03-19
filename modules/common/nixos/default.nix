{ pkgs, ... }: {

  imports = [ ./style.nix ];

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
    dig
    fd
    ffmpeg
    file
    findutils
    fzf
    gawk
    git
    gnugrep
    gnumake
    gnused
    gnutar
    groff
    gzip
    htop
    imagemagick
    killall
    less
    lsof
    ncdu
    openssl
    (parallel-full.override { willCite = true; })
    perl
    postgresql
    python3
    ripgrep
    rsync
    ruby
    sqlite
    time
    tree
    unzip
    vim
    w3m
    watch
    wget
    xclip
    xz
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
