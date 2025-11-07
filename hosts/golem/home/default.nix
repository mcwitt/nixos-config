{ pkgs, ... }:

let
  dpi = 183;
in
{
  imports = [
    ./elfeed-web.nix
  ];

  config = {
    home.stateVersion = "21.11";

    programs.spotify.package = pkgs.spotify.override { deviceScaleFactor = 1.8; };

    programs.rofi.extraConfig.dpi = dpi;

    services.polybar = {
      settings = {
        "bar/main".modules-right = "wired-network filesystem memory cpu temperature pipewire tray";

        "module/temperature" = {
          type = "internal/temperature";
          hwmon-path = "/sys/devices/platform/coretemp.0/hwmon/hwmon2/temp1_input";
        };
      };
    };

    xresources.properties."Xft.dpi" = dpi;
  };
}
