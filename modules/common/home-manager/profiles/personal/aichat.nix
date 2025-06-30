{
  config,
  lib,
  pkgs,
  ...
}:
{
  config = lib.mkIf config.profiles.personal.enable {
    home.packages = [ pkgs.aichat ];
    home.shellAliases.ai = "aichat -e";
  };
}
