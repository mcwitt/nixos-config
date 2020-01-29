{ config, pkgs, ... }:

{
  imports = [ ./fonts.nix ./packages.nix ];

  environment = {
    darwinConfig = "$HOME/.config/nixpkgs/darwin-configuration.nix";
    shells = with pkgs; [ bashInteractive zsh ];
  };

  nix = {
    maxJobs = 8;
    buildCores = 0;

    # Auto upgrade nix package and the daemon service.
    # services.nix-daemon.enable = true;
    # package = pkgs.nix;
  };

  programs = {
    zsh.enable = true;

    tmux = {
      enable = true;
      enableSensible = true;
      enableVim = true;
    };
  };

  services = {
    emacs.enable = true;
    postgresql.enable = true;
  };

  # Used for backwards compatibility, please read the changelog before changing.
  # $ darwin-rebuild changelog
  system = {
    stateVersion = 4;

    defaults.dock = {
      autohide = true;
      launchanim = false;
      orientation = "left";
    };
  };
}
