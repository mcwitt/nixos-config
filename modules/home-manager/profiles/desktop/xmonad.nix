{
  config,
  lib,
  pkgs,
  ...
}:
{
  config = lib.mkIf config.profiles.desktop.enable {

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
                "@colorBase01@"
                "@colorBase03@"
                "@colorBase04@"
                "@colorBase05@"
                "@colorBase08@"
                "@colorBase0D@"
                "@colorBase0F@"
                "@fontMono@"
                "@fontSize@"
              ]
              [
                colors.withHashtag.base01
                colors.withHashtag.base03
                colors.withHashtag.base04
                colors.withHashtag.base05
                colors.withHashtag.base08
                colors.withHashtag.base0D
                colors.withHashtag.base0F
                fonts.monospace.name
                (toString fonts.sizes.desktop)
              ]
              raw;
        in
        pkgs.writeText "xmonad.hs" substituted;
    };
  };
}
