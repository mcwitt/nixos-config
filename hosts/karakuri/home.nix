{ config, pkgs, ... }:
let
  displayWidth = 3840;
  trayMaxIcons = 5;
  statusBarHeight = 36;
in
{
  imports = [
    ../../modules/common/home-manager
    ../../modules/nixos/home-manager
  ];

  programs.xmobar = let cfg = config.programs.xmobar; in
    {
      commands = [
        ''
          Run Battery [ "--template" , "Batt: <acstatus>"
                      , "--Low"      , "10"        -- units: %
                      , "--High"     , "80"        -- units: %
                      , "--low"      , "${cfg.colors.alert}"
                      , "--normal"   , "${cfg.colors.normal}"
                      , "--high"     , "${cfg.colors.good}"

                      , "--" -- battery specific options
                      -- discharging status
                      , "-o"  , "<left>% (<timeleft>)"
                      -- AC "on" status
                      , "-O"  , "<fc=#dAA520>Charging</fc>"
                      -- charged status
                      , "-i"  , "<fc=#006000>Charged</fc>"
                      ] 50
        ''
      ];

      config = {
        position = ''
          Static
            { xpos = 0
            , ypos = 0
            , width = ${toString (displayWidth - trayMaxIcons * statusBarHeight)}
            , height = ${toString statusBarHeight}
            }
        '';
        template = ''"%StdinReader% | %multicpu% | %memory% | %dynnetwork% }{ %battery% | %date% |"'';
      };
    };

  services.blueman-applet.enable = true;

  services.gammastep.provider = "geoclue2";

  services.stalonetray.config = {
    geometry = "${toString trayMaxIcons}x1-0";
    icon_gravity = "NE";
    icon_size = statusBarHeight * 7 / 8;
    slot_size = statusBarHeight;
  };
}
