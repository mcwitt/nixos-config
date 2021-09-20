{ config, pkgs, ... }:
let statusBarHeight = 54;
in
{
  imports = [
    ../../modules/common/home-manager
    ../../modules/nixos/home-manager
  ];

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
    geometry = "5x1+3095";
    icon_gravity = "NE";
    icon_size = statusBarHeight * 7 / 8;
    slot_size = statusBarHeight;
  };

  xsession.pointerCursor.size = 64;
}
