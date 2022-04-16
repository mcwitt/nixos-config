{ pkgs, ... }: {

  imports = [ ./hoogle.nix ];

  i18n.defaultLocale = "en_US.UTF-8";

  nix.extraOptions = ''
    keep-outputs = true
    keep-derivations = true
    experimental-features = nix-command flakes
  '';

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
