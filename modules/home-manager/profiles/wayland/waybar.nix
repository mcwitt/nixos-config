{
  config,
  lib,
  pkgs,
  ...
}:
let
  colors = config.lib.stylix.colors.withHashtag;
in
{
  # Host-overridable modules-right list (mirrors the old polybar modules-right).
  # Workspaces and window title come from Emacs (ewm), not waybar.
  options.programs.waybarModulesRight = lib.mkOption {
    type = lib.types.listOf lib.types.str;
    default = [
      "disk"
      "cpu"
      "memory"
      "network"
      "pulseaudio"
      "tray"
    ];
    description = "waybar modules-right list (host-overridable).";
  };

  config = lib.mkIf (config.profiles.wayland.enable && pkgs.stdenv.isLinux) {
    programs.waybar = {
      enable = true;
      systemd.enable = true; # binds to graphical-session.target (started by ewm.service)
      settings.mainBar = {
        layer = "top";
        position = "top";
        height = 28;
        modules-left = [ ];
        modules-center = [ "clock" ];
        modules-right = config.programs.waybarModulesRight;

        clock = {
          interval = 1;
          format = " {:%Y-%m-%d %a  %H:%M:%S}";
          tooltip-format = "<tt><small>{calendar}</small></tt>";
        };
        cpu = {
          interval = 2;
          format = "  {usage}%";
          on-click = "${lib.getExe pkgs.ghostty} -e ${lib.getExe pkgs.btop}";
        };
        memory = {
          interval = 2;
          format = "  {percentage}%";
          on-click = "${lib.getExe pkgs.ghostty} -e ${lib.getExe pkgs.btop}";
        };
        # hwmon-path is set per host (auto-detection picks the wrong sensor on
        # the desktop boards; the old polybar configs pinned it for the same
        # reason).
        temperature = {
          critical-threshold = 80;
          format = " {temperatureC}°C";
        };
        disk = {
          interval = 30;
          format = "  {percentage_used}%";
          path = "/";
        };
        network = {
          interval = 2;
          format-ethernet = "󰛳  {ifname} {bandwidthDownBits}";
          format-wifi = "  {essid} {signalStrength}%";
          format-disconnected = "󰲛 disconnected";
        };
        battery = {
          states = {
            warning = 30;
            critical = 10;
          };
          format = "  {capacity}%";
          format-charging = "  {capacity}%";
          format-full = "  {capacity}%";
        };
        pulseaudio = {
          format = "󰕾 {volume}%";
          format-muted = "󰝟";
          on-click = lib.getExe pkgs.pavucontrol;
          scroll-step = 5;
        };
        tray = {
          icon-size = 18;
          spacing = 8;
        };
      };
      style = ''
        * {
          font-family: "${config.stylix.fonts.monospace.name}";
          font-size: ${toString config.stylix.fonts.sizes.desktop}pt;
          min-height: 0;
        }
        window#waybar {
          background: ${colors.base00};
          color: ${colors.base05};
        }
        #clock, #cpu, #memory, #temperature, #network, #battery,
        #pulseaudio, #tray, #custom-nvidia {
          padding: 0 8px;
        }
        /* base06 desktop accent */
        #clock { color: ${colors.base06}; }
        #battery.warning { color: ${colors.base09}; }
        #battery.critical { color: ${colors.base08}; }
        #temperature.critical { color: ${colors.base08}; }
      '';
    };
  };
}
