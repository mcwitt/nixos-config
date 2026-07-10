{ pkgs, ... }:
{
  config = {
    home.stateVersion = "25.05";

    programs.spotify.package = pkgs.spotify.override { deviceScaleFactor = 1.8; };

    # Two displays side by side: DP-2 (primary, left) + DP-0 (right). The old
    # X11 config recorded only the outputs, not their resolution; 3840x2160 is
    # assumed (confirm during hardware verification). scale 1.5 ~ the old
    # Xft.dpi 163 perceptual size.
    programs.ewmOutputConfig = ''
      '(("DP-2" :width 3840 :height 2160 :scale 1.5 :x 0 :y 0)
        ("DP-0" :width 3840 :height 2160 :scale 1.5 :x 2560 :y 0))
    '';

    # CPU die sensor; waybar's default thermal_zone0 is the wrong sensor on
    # this board (same reason the old polybar config pinned hwmon-path).
    programs.waybar.settings.mainBar.temperature.hwmon-path = "/sys/class/hwmon/hwmon1/temp1_input";

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
