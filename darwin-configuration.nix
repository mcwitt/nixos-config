{ config, pkgs, ... }:

{
  environment.systemPackages = with pkgs;
    [ aspell
      direnv
      emacs
      gawk
      git
      gnupg
      jq
      imagemagick
      openssh
      openssl
      pandoc
      python3
      ripgrep
      rsync
      texlive.combined.scheme-full
      tree
      vim
      watch
      wget
    ];

  environment.darwinConfig = "$HOME/.config/nixpkgs/darwin-configuration.nix";

  environment.shells = with pkgs; [ bashInteractive zsh ];

  # Auto upgrade nix package and the daemon service.
  # services.nix-daemon.enable = true;
  # nix.package = pkgs.nix;

  programs.zsh.enable = true;

  programs.tmux = {
    enable = true;
    enableSensible = true;
    enableVim = true;
  };

  services.postgresql.enable = true;

  # Used for backwards compatibility, please read the changelog before changing.
  # $ darwin-rebuild changelog
  system.stateVersion = 4;

  system.defaults.dock = {
    autohide = true;
    launchanim = false;
    orientation = "left";
  };

  nix.maxJobs = 8;
  nix.buildCores = 0;
}
