{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.profiles.personal;
in
{
  config = lib.mkIf cfg.enable {

    home.packages = [
      pkgs.sox # for voice mode
    ];

    programs.claude-code = {
      enable = true;
      settings = {
        includeCoAuthoredBy = false;
        model = "claude-opus-4-6";
      };
    };
  };
}
