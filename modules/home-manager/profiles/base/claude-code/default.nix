{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.profiles.base;
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
        voiceEnabled = true;
      };
      skills = {
        nix-init = ./skills/nix-init;
      };
    };
  };
}
