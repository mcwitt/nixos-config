{ config, pkgs, ... }:

{
  imports = [ ./fonts.nix ./packages.nix ];

  environment = {
    darwinConfig = "$HOME/.config/nixpkgs/darwin-configuration.nix";
    shells = with pkgs; [ bashInteractive zsh ];
  };

  fonts.enableFontDir = true;

  nix = {
    maxJobs = 8;
    buildCores = 0;
    package = pkgs.nix;
  };

  nixpkgs = {
    config.allowUnfree = true;
    overlays = (import ./utils.nix).importOverlaysDir ../overlays;
  };

  programs = {
    gnupg.agent.enable = true;

    tmux = {
      enable = true;
      enableSensible = true;
      enableVim = true;
    };

    zsh.enable = true;
  };

  services = {

    emacs = {
      enable = true;
      package = pkgs.mypkgs.emacs;
    };

    nix-daemon.enable = true;
  };

  system = {
    # Used for backwards compatibility, please read the changelog before changing.
    # $ darwin-rebuild changelog
    stateVersion = 4;

    defaults.dock = {
      autohide = true;
      launchanim = false;
      orientation = "left";
    };
  };

  users.users.matt.shell = pkgs.zsh;
}
