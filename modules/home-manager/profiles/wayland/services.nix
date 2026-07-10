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
  config = lib.mkIf (config.profiles.wayland.enable && pkgs.stdenv.isLinux) {

    home.packages = with pkgs; [
      wl-clipboard
      grim
      slurp
      satty
      swaybg
    ];

    # Notifications (dunst replacement). stylix themes the base colors; we only
    # force the base06 accent on the frame, matching the old dunst convention.
    services.mako = {
      enable = true;
      settings = {
        anchor = "top-right";
        border-size = 6;
        border-color = lib.mkForce colors.base06;
        width = 600;
        margin = "12";
        default-timeout = 8000;
      };
    };

    # Idle -> lock, plus the logind Lock signal (ewm binds s-l to
    # `loginctl lock-session`) and lock-before-suspend.
    services.swayidle = {
      enable = true;
      timeouts = [
        {
          timeout = 600;
          command = "${lib.getExe pkgs.swaylock} -f";
        }
      ];
      events = [
        {
          event = "lock";
          command = "${lib.getExe pkgs.swaylock} -f";
        }
        {
          event = "before-sleep";
          command = "${pkgs.systemd}/bin/loginctl lock-session";
        }
      ];
    };

    # swaylock colors are themed by stylix.
    programs.swaylock.enable = true;

    # Wallpaper. Bound to graphical-session.target, which ewm.service activates.
    systemd.user.services.swaybg = {
      Unit = {
        Description = "Wallpaper via swaybg";
        PartOf = [ "graphical-session.target" ];
        After = [ "graphical-session.target" ];
      };
      Install.WantedBy = [ "graphical-session.target" ];
      Service = {
        ExecStart = "${lib.getExe pkgs.swaybg} -m fill -i ${config.stylix.image}";
        Restart = "on-failure";
      };
    };

    # Blue-light filter. On Wayland gammastep uses the wlr-gamma-control
    # protocol; if ewm/Smithay does not implement it this is a no-op (see plan
    # risk R8.2 / hardware verification).
    services.gammastep = {
      enable = true;
      provider = "geoclue2";
      settings.general.brightness-night = 0.6;
    };
  };
}
