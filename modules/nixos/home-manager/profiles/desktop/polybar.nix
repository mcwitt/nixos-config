{ config, lib, pkgs, ... }:
{
  config = lib.mkIf config.profiles.desktop.enable {

    services.polybar = {
      enable = true;

      script = ''
        polybar &
      '';

      settings =
        let
          colors = config.lib.stylix.colors.withHashtag;
          runTermAppOnClick = bin: label: "%{A1:${pkgs.wezterm}/bin/wezterm start ${bin}:}${label}%{A}";
          windowMargin = 8;
        in
        {
          "colors" = {
            background = colors.base00;
            background-alt = colors.base01;
            foreground = colors.base05;
            primary = colors.base0D;
            secondary = colors.base0F;
            alert = colors.base08;
            warn = colors.base0A;
            disabled = colors.base03;
          };

          "bar/main" = {
            monitor = ''''${env:MONITOR:}'';
            width = "100%";
            height = "20pt";

            dpi = 0; # automatically choose dpi

            background = "#E6${config.lib.stylix.colors.base00}"; # 90% opacity
            foreground = ''''${colors.foreground}'';

            line-size = 5;

            border-size = 0;

            padding-left = 0;
            padding-right = 1;

            module-margin = 1;

            separator = "|";
            separator-foreground = ''''${colors.disabled}'';

            font = let inherit (config.stylix) fonts; in [
              "${fonts.monospace.name}:size=${toString fonts.sizes.desktop};5"
              "Iosevka Nerd Font:size=${toString fonts.sizes.desktop};5"
            ];

            modules-left = "xworkspaces xmonad";
            modules-center = "date";
            modules-right = lib.mkDefault "wired-network filesystem memory cpu pulseaudio";

            cursor-click = "pointer";
            cursor-scroll = "ns-resize";

            enable-ipc = true;

            tray-position = "right";
            tray-maxsize = 32;

            wm-restack = "generic";

            override-redirect = true;
          };

          "global/wm" = {
            margin-bottom = windowMargin;
          };

          "module/xworkspaces" = {
            type = "internal/xworkspaces";

            label-active = "%name%";
            label-active-background = ''''${colors.background-alt}'';
            label-active-underline = ''''${colors.primary}'';
            label-active-padding = 1;

            label-occupied = "%name%";
            label-occupied-padding = 1;

            label-urgent = "%name%";
            label-urgent-background = ''''${colors.alert}'';
            label-urgent-padding = 1;

            label-empty = ""; # hide empty workspaces
          };

          "module/xwindow" = {
            type = "internal/xwindow";
            label = "%title%";
            label-maxlen = 80;
          };

          "module/xmonad" = {
            type = "custom/script";
            exec = "${pkgs.xmonad-log}/bin/xmonad-log";
            tail = true;
            format = "  <label>";
          };

          "module/filesystem" = {
            type = "internal/fs";
            interval = 25;

            mount = [ "/" ];

            label-mounted = "%mountpoint% %percentage_used%%";

            label-unmounted = "%mountpoint% not mounted";
            label-unmounted-foreground = ''''${colors.disabled}'';
          };

          "module/pulseaudio" = {
            type = "internal/pulseaudio";

            format-volume = "<ramp-volume> <label-volume>";
            ramp-volume = [ " " ];
            label-volume = "%percentage%%";

            format-muted = "<label-muted>";
            format-muted-prefix = "  ";
            label-muted = "muted";
          };

          "module/memory" = {
            type = "internal/memory";
            interval = 1;
            format-prefix = "  ";
            label = runTermAppOnClick "${pkgs.htop}/bin/htop" "%percentage_used%%";
          };

          "module/cpu" = {
            type = "internal/cpu";
            interval = 1;
            format = "<ramp-coreload>";
            ramp-coreload = [
              "%{F${colors.base0B}}▁%{F-}"
              "%{F${colors.base0B}}▂%{F-}"
              "%{F${colors.base0A}}▃%{F-}"
              "%{F${colors.base0A}}▄%{F-}"
              "%{F${colors.base09}}▅%{F-}"
              "%{F${colors.base09}}▆%{F-}"
              "%{F${colors.base08}}▇%{F-}"
              "%{F${colors.base08}}█%{F-}"
            ];
          };

          "module/wired-network" = {
            type = "internal/network";
            interface-type = "wired";
            interval = 1;

            format-connected-prefix = "  ";
            label-connected = "%ifname% %netspeed%";

            format-disconnected-prefix = "  ";
            label-disconnected = "%ifname% disconnected";
            label-disconnected-foreground = ''''${colors.disabled}'';
          };

          "module/wireless-network" = {
            type = "internal/network";
            interface-type = "wireless";
            interval = 1;

            format-connected = "<ramp-signal> <label-connected>";
            ramp-signal = [ " " ];
            label-connected = "%essid% %netspeed%";

            format-disconnected-prefix = "  ";
            label-disconnected = "disconnected";
            label-disconnected-foreground = ''''${colors.disabled}'';
          };

          "module/date" = {
            type = "internal/date";
            interval = 1;
            date = "%Y-%m-%d %a";
            time = " %H:%M:%S";
            label = "  %date%   %time%";
          };

          "module/battery" = {
            type = "internal/battery";
            battery = "BAT0";
            adapter = "ADP1";

            time-format = "%H:%M";

            ramp-capacity = [
              "  "
              "  "
              "  "
              "  "
              "  "
            ];

            format-discharging = "<ramp-capacity> <label-discharging>";
            label-discharging = "%percentage%% T−%time%";

            format-charging = "<animation-charging> <label-charging>";
            label-charging = "%percentage%% T−%time%";
            animation-charging = [
              "  "
              "  "
              "  "
              "  "
              "  "
            ];

            format-full = "<ramp-capacity> <label-full>";
            label-full = "%percentage%%";

            low-at = 5;
            format-low = "<animation-low> <label-low>";
            label-low = "%percentage%% T−%time%";
            label-low-foreground = ''''${colors.alert}'';
            animation-low = [
              "%{F${colors.base08}}  %{F-}"
              "%{F${colors.base08}}  %{F-}"
            ];
          };

          "settings" = {
            screenchange-reload = true;
            pseudo-transparency = true;
          };
        };
    };
  };
}
