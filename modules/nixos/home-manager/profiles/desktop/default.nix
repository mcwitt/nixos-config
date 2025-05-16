{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.profiles.desktop;
in
{
  imports = [
    ./polybar.nix
    ./rofi.nix
    ./xmonad.nix
  ];

  config = lib.mkIf cfg.enable {
    home.packages = with pkgs; [ xfce.thunar ];

    services.dunst = {
      enable = true;
      settings.global = {
        browser = "${config.programs.chromium.package}/bin/chromium-browser";
        markup = "full";
        max_icon_size = 100;
        text_icon_padding = 10;
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

    services.gpg-agent = {
      pinentry.package = pkgs.pinentry-gtk2;

      # TODO: pinentry-rofi breaks in ssh sessions
      # extraConfig =
      #   let
      #     pinentry-rofi-with-env = pkgs.writeShellApplication {
      #       name = "pinentry-rofi-with-env";
      #       runtimeInputs = with pkgs; [ coreutils rofi ];
      #       text = ''
      #         "${pkgs.pinentry-rofi}/bin/pinentry-rofi" "$@"
      #       '';
      #     };
      #   in
      #   ''
      #     pinentry-program ${pinentry-rofi-with-env}/bin/pinentry-rofi-with-env
      #   '';
    };

    services.picom = {
      enable = false; # XXX
      backend = "glx";
      activeOpacity = 1.0;
      inactiveOpacity = 0.9;
      fade = true;
      fadeDelta = 3;
      shadow = true;
      settings = {
        blur = {
          method = "gaussian";
          size = 10;
          deviation = 5.0;
        };
        blur-background-exclude = [
          "class_g ?= 'zoom'"
        ];
      };
    };

    services.polybar.enable = true;

    services.udiskie = {
      enable = true;
      tray = "always";
    };

    stylix.enable = true;

    xsession = {
      enable = true;
      windowManager.xmonad.enable = true;
    };
  };
}
