{ config, lib, ... }:

let
  dpi = 200;
in
{
  home.stateVersion = "21.11";

  programs.rofi.extraConfig.dpi = dpi;

  services.blueman-applet.enable = true;

  services.polybar.settings = let colors = config.lib.stylix.colors.withHashtag; in {
    "bar/main" = {
      dpi = lib.mkForce dpi; # workaround; auto-detection fails
      modules-right = "wireless-network filesystem memory battery cpu pulseaudio";
    };

    "module/battery" = {
      type = "internal/battery";
      battery = "BAT0";
      adapter = "ADP1";

      time-format = "%H:%M";
      label-charging = "%{F${colors.base0D}}BAT%{F-} %{F${colors.base0B}}AC%{F-} %percentage%% T−%time%";
      label-discharging = "%{F${colors.base0D}}BAT%{F-} %percentage%% T−%time%";
    };
  };

  xresources.properties = {
    "*dpi" = dpi;
    "Xft.dpi" = dpi;
  };
}
