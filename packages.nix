{ pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    (aspellWithDicts (ds: [ ds.en ]))
    bat
    coreutils
    direnv
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
