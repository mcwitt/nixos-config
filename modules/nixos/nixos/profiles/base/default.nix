{
  i18n.defaultLocale = "en_US.UTF-8";

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

  services.locate = {
    enable = true;
    interval = "hourly";
  };

  # needed for thunar to display thumbnail images
  services.tumbler.enable = true;

  virtualisation.docker.enable = true;
}
