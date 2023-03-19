{ config, lib, pkgs, ... }:
{
  services.polybar = {
    enable = true;

    script = ''
      polybar &
    '';

    settings =
      let
        windowMargin = 8;
        inherit (config.lib.stylix) colors;
        runTermAppOnClick = bin: label: "%{A1:${pkgs.wezterm}/bin/wezterm start ${bin}:}${label}%{A}";
      in
      {
        "colors" = {
          background = colors.base01;
          background-alt = colors.base02;
          foreground = colors.base05;
          primary = colors.base0D;
          secondary = colors.base0F;
          alert = colors.base08;
          warn = colors.base0A;
          disabled = colors.base03;
        };

        "bar/main" = rec {
          offset-x = 2 * windowMargin;
          width = "100%:-${toString (2 * offset-x)}";
          height = "20pt";
          radius = 6;

          dpi = 0; # automatically choose dpi

          background = ''''${colors.background}'';
          foreground = ''''${colors.foreground}'';

          line-size = 5;

          border-size = 5;
          border-color = colors.base00;

          padding-left = 0;
          padding-right = 1;

          module-margin = 1;

          separator = "|";
          separator-foreground = ''''${colors.disabled}'';

          font = [ "${config.stylix.fonts.monospace.name}:size=10;5" ];

          modules-left = "xworkspaces xmonad";
          modules-center = "date";
          modules-right = "network filesystem memory cpu pulseaudio";

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
        };

        "module/filesystem" = {
          type = "internal/fs";
          interval = 25;

          mount = [ "/" ];

          label-mounted = "%{F${colors.base0D}}%mountpoint%%{F-} %percentage_used%%";

          label-unmounted = "%mountpoint% not mounted";
          label-unmounted-foreground = ''''${colors.disabled}'';
        };

        "module/pulseaudio" = {
          type = "internal/pulseaudio";

          format-volume-prefix = "VOL ";
          format-volume-prefix-foreground = ''''${colors.primary}'';
          format-volume = "<label-volume>";

          label-volume = "%percentage%%";

          label-muted = "muted";
          label-muted-foreground = ''''${colors.disabled}'';

        };

        "module/memory" = {
          type = "internal/memory";
          interval = 1;
          format-prefix = "RAM ";
          format-prefix-foreground = ''''${colors.primary}'';
          label = runTermAppOnClick "${pkgs.htop}/bin/htop" "%percentage_used:2%%";
        };

        "module/cpu" = {
          type = "internal/cpu";
          interval = 1;
          format-prefix-foreground = ''''${colors.primary}'';
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
          label = runTermAppOnClick "${pkgs.htop}/bin/htop" "%percentage-sum:3%%";
        };

        "module/network" = {
          type = "internal/network";
          interface-type = "wired";
          interval = 1;
          label-connected = "%{F${colors.base0D}}%ifname%%{F-} %netspeed:8%";
          label-disconnected = "%{F${colors.base0D}}%ifname%%{F${colors.base03}} disconnected";
        };

        "module/date" = {
          type = "internal/date";
          interval = 1;

          date = "%Y-%m-%d %a %H:%M:%S";

          label = "%date%";
          label-foreground = ''''${colors.primary}'';
        };

        "settings" = {
          screenchange-reload = true;
          pseudo-transparency = true;
        };
      };
  };
}
