{ lib, ... }:
let statusBarHeight = 36;
in
{
  home.stateVersion = "21.11";

  programs.rofi.font = lib.mkForce "Iosevka Comfy 20"; # HACK since dpi setting in Xresources seems ignored

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

  services.stalonetray.config = {
    geometry = "5x1+3250";
    icon_gravity = "NE";
    icon_size = statusBarHeight * 7 / 8;
    slot_size = statusBarHeight;
  };

  xresources.properties = {
    "*dpi" = 183;
    "Xft.dpi" = 183;
  };
}
