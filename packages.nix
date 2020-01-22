{ pkgs, ... }:

{ environment.systemPackages = with pkgs; [
    aspell
    coreutils
    direnv
    emacs
    fzf
    gawk
    git
    gnugrep
    gnumake
    gnupg
    gnused
    gnutar
    gzip
    imagemagick
    jq
    less
    openssh
    openssl
    pandoc
    perl
    python3
    ripgrep
    rsync
    ruby
    texlive.combined.scheme-full
    time
    tree
    unzip
    vim
    w3m
    watch
    wget
    xz
  ];
}
