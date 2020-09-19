{ config, pkgs, ... }:

{
  imports = [ ./fonts.nix ./packages.nix ];

  environment = {
    darwinConfig = "$HOME/.config/nixpkgs/darwin-configuration.nix";
    shells = with pkgs; [ bashInteractive fish zsh ];
  };

  environment.systemPackages = with pkgs; [ lorri mypkgs.emacs ];

  fonts.enableFontDir = true;

  nix = {
    maxJobs = 8;
    buildCores = 0;
    package = pkgs.nix;
  };

  nixpkgs = {
    config.allowUnfree = true;
    overlays = (import ./utils.nix).importOverlaysDir ./overlays;
  };

  programs.gnupg.agent.enable = true;

  programs.tmux = {
    enable = true;
    enableSensible = true;
    enableVim = true;
  };

  programs.fish.enable = true;

  services.emacs = {
    enable = true;
    package = pkgs.mypkgs.emacs;
  };

  services.nix-daemon.enable = true;

  system.stateVersion = 4;

  system.defaults.dock = {
    autohide = true;
    launchanim = false;
    orientation = "left";
  };

  system.defaults.NSGlobalDomain.ApplePressAndHoldEnabled = false;
}
