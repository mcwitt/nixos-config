{ config, lib, pkgs, ... }:
{
  imports = [ ./hoogle.nix ];

  i18n.defaultLocale = "en_US.UTF-8";

  nix = {
    package = pkgs.nixFlakes;

    extraOptions = ''
      keep-outputs = true
      keep-derivations = true
    '' + lib.optionalString (config.nix.package == pkgs.nixFlakes) "experimental-features = nix-command flakes";
  };

  nixpkgs = {
    config.allowUnfree = true;
    overlays = import ../../../../../overlays;
  };

  programs.fish.enable = true;

  programs.mtr.enable = true;

  programs.gnupg.agent = {
    enable = true;
    enableSSHSupport = true;
  };

  services.avahi = {
    enable = true;
    nssmdns = true;
  };

  services.locate = {
    enable = true;
    localuser = null;
    locate = pkgs.mlocate;
    interval = "hourly";
  };

  # needed for thunar to display thumbnail images
  services.tumbler.enable = true;

  virtualisation.docker.enable = true;
}
