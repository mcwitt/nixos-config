{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.profiles.base;

  autoresearch = pkgs.fetchFromGitHub {
    owner = "uditgoenka";
    repo = "autoresearch";
    rev = "0a1b6779cb817314a31caea51512e5aa07219dbe";
    hash = "sha256-FHCJ5Kika4PflJWACdml+ok5PMWQTytchH2va/3bmh0=";
  };

  autoresearchPlugin = "${autoresearch}/claude-plugin";

in
{
  config = lib.mkIf cfg.enable {

    home.packages = [
      pkgs.sox # for voice mode
    ];

    programs.claude-code = {
      enable = true;
      settings = {
        defaultMode = "auto";
        includeCoAuthoredBy = false;
        model = "claude-opus-4-6";
        voiceEnabled = true;
      };
      skills = {
        autoresearch = "${autoresearchPlugin}/skills/autoresearch";
        nix-init = ./skills/nix-init;
      };
      commandsDir = "${autoresearchPlugin}/commands";
    };
  };
}
