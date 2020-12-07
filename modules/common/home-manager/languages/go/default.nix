{ config, lib, pkgs, ... }:
with lib;
let cfg = config.languages.go;
in
{
  options.languages.go.enable = mkEnableOption "Go language environment";

  config = mkIf cfg.enable {
    home.packages = [ pkgs.golint ];
    programs.go.enable = true;
    programs.emacs.init.usePackage.go-mode.enable = true;
  };
}
