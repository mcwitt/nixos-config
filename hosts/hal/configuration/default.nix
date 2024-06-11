{ pkgs, ... }:

{
  imports = [ ./hardware-configuration.nix ];

  boot = {
    kernelPackages = pkgs.linuxKernel.packages.linux_rpi4;
    initrd.availableKernelModules = [ "xhci_pci" "usbhid" "usb_storage" ];
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

  profiles.home-automation.enable = true;

  time.timeZone = "America/Los_Angeles";

  services.xserver.xkb.layout = "us";

  users.users.matt = {
    isNormalUser = true;
    extraGroups = [ "wheel" ];
  };

  environment.systemPackages = with pkgs; [
    home-assistant-cli
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

  services.zigbee2mqtt.settings.serial.port = "/dev/serial/by-id/usb-ITEAD_SONOFF_Zigbee_3.0_USB_Dongle_Plus_V2_20230602161357-if00";

  system.stateVersion = "23.11";

  hardware.enableRedistributableFirmware = true;
}
