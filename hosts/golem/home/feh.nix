# Override stylix module to use pass --bg-max rather than --bg-scale to feh
{ config, pkgs, ... }:
{
  stylix.targets.feh.enable = false;

  xsession.initExtra = ''
    ${pkgs.feh}/bin/feh --no-fehbg --bg-max ${config.stylix.image}
  '';
}
