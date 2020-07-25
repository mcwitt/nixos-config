{ pkgs, ... }: {
  environment.systemPackages = [ pkgs.cudatoolkit_10_2 ];

  hardware = {
    opengl.driSupport32Bit = true;
    nvidia.modesetting.enable = true;
  };

  nixpkgs.config.allowUnfree = true;
  services.xserver.videoDrivers = [ "nvidia" ];
}
