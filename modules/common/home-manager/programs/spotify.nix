{ config, lib, pkgs, ... }:
{
  options.programs.spotify = {
    enable = lib.mkEnableOption "Spotify desktop application";

    package = lib.mkOption {
      type = lib.types.package;
      default = pkgs.spotify;
      defaultText = "pkgs.spotify";
      example = lib.literalExpression "pkgs.spotify.override { deviceScaleFactor = 1.5; }";
      description = "The spotify package to use";
    };
  };

  config = let cfg = config.programs.spotify; in lib.mkIf cfg.enable {
    home.packages = [ cfg.package ];
  };
}
