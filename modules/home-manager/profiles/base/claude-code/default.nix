{
  config,
  inputs,
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
        model = "claude-opus-4-7";
        effortLevel = "xhigh";
        defaultMode = "auto";
        editorMode = "vim";
        voiceEnabled = true;
      };

      plugins = with inputs; [
        "${autoresearch}/claude-plugin"
        superpowers
      ];

      skills = {
        nix-init = ./skills/nix-init;
      };
    };
  };
}
