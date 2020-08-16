{ pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    (aspellWithDicts (ds: [ ds.en ]))
    bat
    coreutils
    direnv
    ffmpeg
    findutils
    fzf
    gawk
    git
    gnugrep
    gnumake
    gnupg
    gnused
    gnutar
    gzip
    htop
    imagemagick
    jq
    killall
    less
    lsof
    nodejs
    openssh
    openssl
    parallel
    perl
    python3
    ripgrep
    rsync
    ruby
    sqlite
    texlive.combined.scheme-full
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
