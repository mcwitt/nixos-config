{ pkgs, ... }: {

  imports = [ ./home-assistant.nix ./hoogle.nix ];

  i18n.defaultLocale = "en_US.UTF-8";

  nix = {
    extraOptions = ''
      keep-outputs = true
      keep-derivations = true
      experimental-features = nix-command flakes
    '';

    settings = {
      substituters = [
        "https://nix-community.cachix.org"
      ];
      trusted-public-keys = [
        "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
      ];
    };
  };

  nixpkgs.config.pulseaudio = true;

  programs.fish.enable = true;

  programs.mtr.enable = true;

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

  services.openssh.enable = true;

  # needed for thunar to display thumbnail images
  services.tumbler.enable = true;

  services.udisks2.enable = true;

  virtualisation.docker.enable = true;
}
