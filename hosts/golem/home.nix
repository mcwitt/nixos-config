{ pkgs, ... }:
let statusBarHeight = 36;
in
{
  imports = [
    ../../modules/common/home-manager
    ../../modules/nixos/home-manager
  ];

  programs.xmobar = {
    enable = true;
    rc.extraConfig = {
      position = ''
        Static
          { xpos = 0
          , ypos = 0
          , width = 3840
          , height = ${toString statusBarHeight}
          }
      '';
      template = ''"%StdinReader% | %multicpu% | %coretemp% | %memory% | %dynnetwork% }{| %date% "'';
    };
  };

  services.gammastep = {
    latitude = 37.8044;
    longitude = -122.2712;
  };

  services.stalonetray.config = {
    geometry = "5x1-2495";
    icon_gravity = "NE";
    icon_size = statusBarHeight * 7 / 8;
    slot_size = statusBarHeight;
  };
}
