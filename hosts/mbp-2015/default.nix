{ pkgs, ... }:

{
  imports = [
    <home-manager/nix-darwin>
    ../../modules/common/nixos
  ];

  environment = {
    darwinConfig = "$HOME/.config/nixpkgs/darwin-configuration.nix";
    shells = with pkgs; [ bashInteractive fish zsh ];
  };

  fonts.enableFontDir = true;

  home-manager = {
    useGlobalPkgs = true;
    users.matt = {
      imports = [
        ../../modules/common/home-manager
        ../../modules/darwin/home-manager
      ];
    };
  };

  nix = {
    maxJobs = 8;
    buildCores = 0;
    package = pkgs.nix;
  };

  nixpkgs = {
    config.allowUnfree = true;
    overlays = import ../../overlays;
  };

  programs.bash.enable = true;

  programs.gnupg.agent.enable = true;

  programs.tmux = {
    enable = true;
    enableSensible = true;
    enableVim = true;
  };

  programs.fish.enable = true;

  programs.zsh.enable = true;

  services.nix-daemon.enable = true;

  system.stateVersion = 4;

  system.defaults.dock = {
    autohide = true;
    launchanim = false;
    orientation = "left";
    tilesize = 48;
  };

  system.defaults.NSGlobalDomain.ApplePressAndHoldEnabled = false;

  users.users.matt.shell = pkgs.zsh;
}
