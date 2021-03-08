{ pkgs, ... }:
let
  leftMonitorWidth = 3840;
  rightMonitorWidth = 2160;
  trayMaxIcons = 5;
  statusBarHeight = 36;
in
{
  imports = [
    ../../modules/common/home-manager
    ../../modules/nixos/home-manager
  ];

  programs.xmobar = {
    commands = [
      ''
        Run Weather "KSFO" [ "--template"
                           , "<skyCondition> | <fc=#268bd2><tempF></fc>°F | <fc=#268bd2><rh></fc>% | <fc=#268bd2><pressure></fc>hPa"
                           ] 36000
      ''
    ];
    config = {
      position = ''
        Static
          { xpos = 0
          , ypos = 0
          , width = ${toString (leftMonitorWidth - trayMaxIcons * statusBarHeight)}
          , height = ${toString statusBarHeight}
          }
      '';
      template = ''"%StdinReader% | %multicpu% | %coretemp% | %memory% | %dynnetwork% }{ %KSFO% | %date% |"'';
    };
  };

  services.gammastep = {
    latitude = 37.8044;
    longitude = -122.2712;
  };

  services.stalonetray.config = {
    geometry = "${toString trayMaxIcons}x1-${toString rightMonitorWidth}";
    icon_gravity = "NE";
    icon_size = statusBarHeight * 7 / 8;
    slot_size = statusBarHeight;
  };
}
