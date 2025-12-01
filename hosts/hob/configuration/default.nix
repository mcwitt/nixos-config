{ inputs, pkgs, ... }:

{
  imports = with inputs.nixos-raspberrypi.nixosModules.raspberry-pi-5; [
    base
    display-vc4
  ];

  # https://github.com/nix-community/disko?tab=readme-ov-file#sample-configuration-and-cli-command
  disko.devices.disk = {
    gpt = {
      device = "/dev/sda";
      type = "disk";
      content = {
        type = "gpt";
        partitions = {
          ESP = {
            type = "EF00";
            size = "512M";
            content = {
              type = "filesystem";
              format = "vfat";
              mountpoint = "/boot/firmware";
              mountOptions = [
                "umask=0077"
                "noatime"
                "noauto"
                "x-systemd.automount"
                "x-systemd.idle-timeout=1min"
              ];
            };
          };
          swap = {
            size = "8G";
            content = {
              type = "swap";
            };
          };
          root = {
            size = "100%";
            content = {
              type = "filesystem";
              format = "ext4";
              mountpoint = "/";
              mountOptions = [ "noatime" ];
            };
          };
        };
      };
    };
  };

  boot.loader.raspberryPi.bootloader = "kernel";

  networking.hostName = "hob";

  nix.gc.automatic = true;
  nix.gc.dates = "weekly";

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
