{ config, pkgs, ... }:

{
  imports = [ ./fonts.nix ./packages.nix ];

  environment = {
    darwinConfig = "$HOME/.config/nixpkgs/darwin-configuration.nix";
    shells = with pkgs; [ bashInteractive zsh ];
  };

  # Auto upgrade nix package and the daemon service.
  # services.nix-daemon.enable = true;
  # nix.package = pkgs.nix;

  programs.zsh.enable = true;

  programs.tmux = {
    enable = true;
    enableSensible = true;
    enableVim = true;
  };

  services.emacs.enable = true;
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
