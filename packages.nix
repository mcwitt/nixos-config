{ pkgs, ... }:

{ environment.systemPackages = with pkgs; [
    aspell
    coreutils
    direnv
    emacs
    fzf
    gawk
    git
    gnupg
    gnugrep
    gnumake
    gnused
    gnutar
    gzip
    jq
    imagemagick
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
    watch
    wget
    xz
  ];
}
