{ config, lib, ... }:
with lib;
let
  cfg = config.languages.go;
in
{
  options.languages.go.enable = mkEnableOption "Go language environment";

  config = mkIf cfg.enable {

    programs.go.enable = true;

    programs.emacs.init.usePackage.go-mode.enable = true;
  };
}
