{ config, lib, ... }:
let statusBarHeight = 54;
in
{
  imports = [
    ../../../modules/common/home-manager
    ../../../modules/nixos/home-manager
  ];

  home.pointerCursor = {
    size = 64;
    x11.enable = true;
  };

  programs.rofi.font = lib.mkForce "Iosevka Comfy 24"; # HACK since dpi setting in Xresources seems ignored

  programs.xmobar.rc = let cfg = config.programs.xmobar.rc; in
    {
      commands = [
        ''
          Run Cpu     [ "--Low"      , "50"
                      , "--High"     , "85"
                      , "--low"      , "${cfg.colors.good}"
                      , "--normal"   , "${cfg.colors.normal}"
                      , "--high"     , "${cfg.colors.alert}"
                      , "--ppad"     , "3"
                      ] 10
        ''
        ''
          Run Battery [ "--template" , "Batt: <acstatus>"
                      , "--Low"      , "10"
                      , "--High"     , "80"
                      , "--low"      , "${cfg.colors.alert}"
                      , "--normal"   , "${cfg.colors.normal}"
                      , "--high"     , "${cfg.colors.good}"

                      , "--" -- battery specific options
                      -- discharging status
                      , "-o"  , "<left>% (<timeleft>)"
                      -- AC "on" status
                      , "-O"  , "<fc=${cfg.colors.normal}>Charging</fc>"
                      -- charged status
                      , "-i"  , "<fc=${cfg.colors.good}>Charged</fc>"
                      ] 50
        ''
      ];

      extraConfig = {
        font = ''"xft:Iosevka:size=12:bold:antialias=true"'';
        position = ''
          Static
            { xpos = 0
            , ypos = 0
            , width = 3840
            , height = ${toString statusBarHeight}
            }
        '';
        template = ''"%StdinReader% | %cpu% | %memory% | %battery% | %dynnetwork% }{| %date%"'';
      };
    };

  services.blueman-applet.enable = true;

  services.gammastep.provider = "geoclue2";

  services.stalonetray.config = {
    geometry = "5x1+3100";
    icon_gravity = "NE";
    icon_size = statusBarHeight * 7 / 8;
    slot_size = statusBarHeight;
  };

  xresources.properties = {
    "*dpi" = 200;
    "Xft.dpi" = 200;
  };
}
