{ pkgs, ... }:
{
  config = {
    home.stateVersion = "21.11";

    programs.spotify.package = pkgs.spotify.override { deviceScaleFactor = 1.8; };

    # DP-4 (primary, left) + DP-2 rotated left (the old xrandr "Rotate left").
    # xrandr "left" is a counter-clockwise 90deg, which in wl_output/Smithay
    # transform terms is 3 (270); transform 1 would come up upside-down.
    # Resolutions assumed 3840x2160; confirm rotation + resolution during
    # hardware verification. scale 1.5 ~ the old Xft.dpi 183.
    programs.ewmOutputConfig = ''
      '(("DP-4" :width 3840 :height 2160 :scale 1.5 :x 0 :y 0)
        ("DP-2" :width 3840 :height 2160 :scale 1.5 :transform 3 :x 2560 :y 0))
    '';

    # CPU package sensor; waybar's default thermal_zone0 is the wrong sensor on
    # this board (same reason the old polybar config pinned hwmon-path).
    programs.waybar.settings.mainBar.temperature.hwmon-path =
      "/sys/devices/platform/coretemp.0/hwmon/hwmon2/temp1_input";

    programs.waybarModulesRight = [
      "network"
      "disk"
      "memory"
      "cpu"
      "temperature"
      "custom/nvidia"
      "pulseaudio"
      "tray"
    ];
  };
}
