{
  config,
  inputs,
  lib,
  pkgs,
  ...
}:
{

  config = lib.mkIf config.profiles.base.enable {
    i18n.defaultLocale = "en_US.UTF-8";

    nix = {
      extraOptions = ''
        keep-outputs = true
        keep-derivations = true
        experimental-features = nix-command flakes
      '';

      registry.nixpkgs.flake = inputs.nixpkgs;
    };

    programs.fish.enable = true;

    programs.mtr.enable = true;

    programs.nix-ld.enable = true;

    services.avahi = {
      enable = true;
      nssmdns4 = true;
    };

    services.earlyoom = {
      enable = true;
      enableNotifications = true;
    };

    services.hoogle = {
      enable = true;
      port = 8081;
      packages =
        ps: with ps; [
          aeson
          # array
          # containers
          lens
          lens-aeson
          monad-loops
          # mtl
          optparse-generic
          # parsec
          random-fu
          rvar
          safe
          split
          streaming
          # text
          turtle
          vector
        ];
    };

    services.locate = {
      enable = true;
      package = pkgs.mlocate;
      interval = "hourly";
    };

    services.openssh.enable = true;

    # needed for thunar to display thumbnail images
    services.tumbler.enable = true;

    services.udisks2.enable = true;

    virtualisation.podman = {
      enable = true;

      # Create an alias mapping docker to podman
      dockerCompat = true;

      # Required for containers under podman-compose to be able to talk to each other
      defaultNetwork.settings.dns_enabled = true;
    };
  };
}
