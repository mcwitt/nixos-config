{ pkgs, ... }: {

  nixpkgs.config = {
    allowUnfree = true;
    joypixels.acceptLicense = true;
  };

  fonts = {
    enableDefaultFonts = true;
    fonts = with pkgs; [
      emacs-all-the-icons-fonts
      fira-code
      fira-code-symbols
      iosevka
      joypixels
      nerdfonts
      source-code-pro
    ];
  };

  environment.systemPackages = with pkgs; [
    (aspellWithDicts (ds: [ ds.en ]))
    coreutils
    fd
    ffmpeg
    file
    findutils
    fzf
    gawk
    git
    gnugrep
    gnumake
    gnupg
    gnused
    gnutar
    groff
    gzip
    htop
    imagemagick
    jq
    killall
    less
    lsof
    ncdu
    openssh
    openssl
    parallel
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
}
