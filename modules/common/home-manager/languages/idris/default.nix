{ config, lib, pkgs, ... }:
with lib;
let cfg = config.languages.idris;
in
{
  options.languages.idris.enable = mkEnableOption "Idris language environment";

  config = mkIf cfg.enable {

    home.packages = [ pkgs.idris2 ];

    programs.emacs.init.usePackage.idris-mode.enable = true;

    programs.neovim.plugins = [ pkgs.vimPlugins.idris2-vim ];
  };
}
