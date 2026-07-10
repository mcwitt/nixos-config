{
  config,
  lib,
  pkgs,
  ...
}:
{
  config = lib.mkIf (config.profiles.x11.enable && pkgs.stdenv.isLinux) {

    xsession.windowManager.xmonad = {
      enable = true;

      enableContribAndExtras = true;

      extraPackages = ps: with ps; [ dbus ];

      config =
        let
          inherit (config.stylix) fonts;
          inherit (config.lib.stylix) colors;
          raw = builtins.readFile ./xmonad/xmonad.hs;
          substituted =
            builtins.replaceStrings
              [
                "@colorBase00@"
                "@colorBase01@"
                "@colorBase03@"
                "@colorBase04@"
                "@colorBase05@"
                "@colorBase06@"
                "@colorBase08@"
                "@fontMono@"
                "@fontSize@"
                "@notifySend@"
                "@wpctl@"
              ]
              [
                colors.withHashtag.base00
                colors.withHashtag.base01
                colors.withHashtag.base03
                colors.withHashtag.base04
                colors.withHashtag.base05
                colors.withHashtag.base06
                colors.withHashtag.base08
                fonts.monospace.name
                (toString fonts.sizes.desktop)
                "${pkgs.libnotify}/bin/notify-send"
                "${pkgs.wireplumber}/bin/wpctl"
              ]
              raw;
        in
        pkgs.writeText "xmonad.hs" substituted;
    };
  };
}
