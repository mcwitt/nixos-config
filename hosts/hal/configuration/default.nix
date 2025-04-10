{ inputs, pkgs, ... }:

{
  imports = [
    ./hardware-configuration.nix
    "${inputs.nixos-hardware}/raspberry-pi/4"
    ./bluetooth.nix
  ];

  boot = {
    kernelPackages = pkgs.linuxKernel.packages.linux_rpi4;
    initrd.availableKernelModules = [
      "xhci_pci"
      "usbhid"
      "usb_storage"
    ];
    loader = {
      grub.enable = false;
      generic-extlinux-compatible.enable = true;
    };
  };

  i18n.defaultLocale = "en_US.UTF-8";

  networking.hostName = "hal";

  nix = {
    gc.automatic = true;
    gc.dates = "weekly";
    settings.trusted-public-keys = [ "golem:eibXP6qvkaDB9Jvh/MkR4D/dVL7HYDBJI2srJZgVhGE=" ];
  };

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

  system.stateVersion = "23.11";

  hardware.enableRedistributableFirmware = true;

  # Enable GPU acceleration
  hardware.raspberry-pi."4".fkms-3d.enable = true;
}
