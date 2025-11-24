{
  config,
  lib,
  pkgs,
  ...
}:
{
  config = lib.mkIf config.profiles.desktop.enable {

    services.polybar = {
      enable = true;

      script = ''
        polybar main &
      '';

      settings =
        let
          colors = config.lib.stylix.colors.withHashtag;
          runTermAppOnClick = bin: label: "%{A1:${lib.getExe pkgs.wezterm} start ${bin}:}${label}%{A}";
          windowMargin = 8;
        in
        {
          "colors" = {
            background = colors.base00;
            background-alt = colors.base01;
            foreground = colors.base05;
            primary = colors.base0F;
            secondary = colors.base0E;
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

            line-size = 4;

            border-size = 0;

            padding-left = 0;
            padding-right = 1;

            module-margin = 1;

            separator = "|";
            separator-foreground = ''''${colors.disabled}'';

            font =
              let
                inherit (config.stylix) fonts;
              in
              [
                "${fonts.monospace.name}:size=${toString fonts.sizes.desktop};5"
                "${fonts.sansSerif.name}:size=${toString fonts.sizes.desktop};5"
              ];

            modules-left = "xworkspaces xmonad";
            modules-center = "date";
            modules-right = lib.mkDefault "wired-network filesystem memory cpu pipewire tray";

            cursor-click = "pointer";
            cursor-scroll = "ns-resize";

            enable-ipc = true;

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
            label-maxlen = 40;
          };

          "module/xmonad" = {
            type = "custom/script";
            exec = lib.getExe pkgs.xmonad-log;
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

            warn-percentage = 75;

            format-warn-foreground = ''''${colors.alert}'';
          };

          "module/pipewire" =
            let
              # https://github.com/polybar/polybar-scripts/blob/8a6a2c7fc6beb281515f81ccf5b9fafc830a3230/polybar-scripts/pipewire-simple/pipewire-simple.sh
              script = pkgs.writeShellScript "pipewire.sh" ''
                PATH=${
                  lib.makeBinPath (
                    with pkgs;
                    [
                      coreutils
                      pamixer
                    ]
                  )
                }

                VOLUME=$(pamixer --get-volume-human)

                case $1 in
                    "--up")
                        pamixer --increase 10
                        ;;
                    "--down")
                        pamixer --decrease 10
                        ;;
                    "--mute")
                        pamixer --toggle-mute
                        ;;
                    *)
                        echo "''${VOLUME}"
                esac
              '';
            in
            {
              type = "custom/script";
              exec = "${script} update";

              interval = 1;
              click-right = "exec ${lib.getExe pkgs.pavucontrol} &";
              click-left = "${script} --mute &";
              scroll-up = "${script} --up &";
              scroll-down = "${script} --down &";

              label = "%output:4%";
              format-prefix = "󰕾 ";
            };

          "module/memory" = {
            type = "internal/memory";
            interval = 1;
            label = runTermAppOnClick (lib.getExe pkgs.btop) "%percentage_used:2%%";
            format-prefix = "  ";
            warn-percentage = 90;
            format-warn-foreground = ''''${colors.alert}'';
          };

          "module/cpu" = {
            type = "internal/cpu";
            label = runTermAppOnClick (lib.getExe pkgs.btop) "%percentage-sum:3%%";
            format-prefix = "  ";
            # ramp-coreload = [
            #   "%{F${colors.base0B}}▁%{F-}"
            #   "%{F${colors.base0B}}▂%{F-}"
            #   "%{F${colors.base0A}}▃%{F-}"
            #   "%{F${colors.base0A}}▄%{F-}"
            #   "%{F${colors.base09}}▅%{F-}"
            #   "%{F${colors.base09}}▆%{F-}"
            #   "%{F${colors.base08}}▇%{F-}"
            #   "%{F${colors.base08}}█%{F-}"
            # ];
          };

          "module/wired-network" = {
            type = "internal/network";
            interface-type = "wired";
            interval = 1;

            label-connected = "󰛳  %ifname% %netspeed:9%";
            label-disconnected = "󰲛  %ifname% disconnected";
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
            label = "  %date% %time%";
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
            format-charging-foreground = colors.base0D;
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

            low-at = 10;
            format-low = "<animation-low> <label-low>";
            label-low = "%percentage%% T−%time%";
            label-low-foreground = ''''${colors.alert}'';
            animation-low = [
              "%{F${colors.base08}}  %{F-}"
              "%{F${colors.base08}}  %{F-}"
            ];
          };

          "module/tray" = {
            type = "internal/tray";
            format-margin = "8px";
            tray-spacing = "8px";
          };

          "settings" = {
            screenchange-reload = true;
            pseudo-transparency = true;
          };
        };
    };
  };
}
