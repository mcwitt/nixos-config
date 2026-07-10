{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.profiles.x11;
in
{
  options.profiles.x11.enable = lib.mkEnableOption "X11/xmonad session (window manager, bar, launcher, compositor)";

  imports = [
    ./polybar.nix
    ./rofi.nix
    ./xmonad.nix
  ];

  config = lib.mkIf (cfg.enable && pkgs.stdenv.isLinux) {

    home.packages = [ pkgs.xclip ];

    programs.feh.enable = true;

    programs.urxvt = {
      enable = true;
      fonts =
        let
          inherit (config.stylix) fonts;
        in
        [ "xft:${fonts.monospace.name}:size=${toString fonts.sizes.applications}:antialias=true" ];
    };

    services.dunst = {
      enable = true;
      settings =
        let
          colors = config.lib.stylix.colors.withHashtag;
        in
        {
          global = {
            browser = "${config.programs.chromium.package}/bin/chromium-browser";
            markup = "full";
            max_icon_size = 100;
            text_icon_padding = 10;
            scale = 1;
            frame_width = 6;
            origin = "top-right";
            offset = "12x58";
            width = 600;
          };

          urgency_low.foreground = lib.mkForce colors.base04;

          urgency_normal = {
            frame_color = lib.mkForce colors.base06;
            highlight = lib.mkForce colors.base06;
            foreground = lib.mkForce colors.base06;
          };

          urgency_critical.foreground = lib.mkForce colors.base08;
        };
    };

    services.flameshot = {
      enable = true;
      settings.General.showStartupLaunchMessage = false;
    };

    services.gammastep = {
      enable = true;
      tray = true;
      provider = "geoclue2";
      settings.general = {
        adjustment-method = "randr";
        brightness-night = 0.6;
      };
    };

    services.picom = {
      enable = true;
      backend = "glx";
      vSync = true;
      shadow = true;
      settings = {
        crop-shadow-to-monitor = true;
        corner-radius = 8;
        rounded-corners-exclude = [ "window_type = 'dock'" ];
      };
    };

    services.polybar.enable = true;

    xsession = {
      enable = true;
      windowManager.xmonad.enable = true;
    };
  };
}
