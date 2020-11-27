{ pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    (aspellWithDicts (ds: [ ds.en ]))
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
