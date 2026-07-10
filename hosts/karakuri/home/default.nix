{ pkgs, ... }:
{
  home.stateVersion = "21.11";

  programs.spotify.package = pkgs.spotify.override { deviceScaleFactor = 2.0; };

  # ThinkPad X1 internal panel. scale 2.0 ~ the old Xft.dpi 224. Docked external
  # outputs hotplug in; add entries here once their connector names are known
  # (ewm / wlr-randr reports them, e.g. "DP-3"). Resolution 3840x2400 assumed for
  # the X1 panel; confirm during hardware verification.
  programs.ewmOutputConfig = ''
    '(("eDP-1" :width 3840 :height 2400 :scale 2.0 :x 0 :y 0))
  '';

  programs.waybarModulesRight = [
    "network"
    "disk"
    "memory"
    "cpu"
    "battery"
    "pulseaudio"
    "tray"
  ];
}
