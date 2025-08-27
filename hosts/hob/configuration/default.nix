{ inputs, pkgs, ... }:

{
  imports = [
    ./hardware-configuration.nix
  ]
  ++ (with inputs.nixos-raspberrypi.nixosModules.raspberry-pi-5; [
    base
    display-vc4
  ]);

  networking.hostName = "hob";

  nix.gc.automatic = true;
  nix.gc.dates = "weekly";
  nix.settings.trusted-public-keys = [ "golem:eibXP6qvkaDB9Jvh/MkR4D/dVL7HYDBJI2srJZgVhGE=" ];

  time.timeZone = "America/Los_Angeles";

  users.users.matt = {
    isNormalUser = true;
    extraGroups = [ "wheel" ];
  };

  environment.systemPackages = with pkgs; [
    vim
  ];

  services.avahi = {
    enable = true;
    nssmdns4 = true;
    publish = {
      enable = true;
      addresses = true;
    };
  };

  services.earlyoom.enable = true;

  services.openssh = {
    enable = true;
    settings.PasswordAuthentication = false;
  };

  # https://docs.paperless-ngx.com/setup/#less-powerful-devices
  services.paperless.settings = {
    PAPERLESS_TASK_WORKERS = 2;
    PAPERLESS_THREADS_PER_WORKER = 1;
    PAPERLESS_CONVERT_MEMORY_LIMIT = "1gb";
  };

  system.stateVersion = "24.11";
}
