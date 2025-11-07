{ pkgs, ... }:

let
  dpi = 183;
in
{
  imports = [
    ./elfeed-web.nix
  ];

  config = {
    home.stateVersion = "25.05";

    programs.spotify.package = pkgs.spotify.override { deviceScaleFactor = 1.8; };

    programs.rofi.extraConfig.dpi = dpi;

    services.polybar = {
      settings = {
        "bar/main".modules-right =
          "wired-network filesystem memory cpu temperature nvidia-gpu pipewire tray";

        "module/temperature" = {
          type = "internal/temperature";
          hwmon-path = "/sys/class/hwmon/hwmon1/temp1_input";
        };
      };
    };

    xresources.properties."Xft.dpi" = dpi;
  };
}
