{ lib, config, pkgs, ... }:
with lib;
let cfg = config.languages.unison;
in
{
  options.languages.unison.enable = mkEnableOption "Unison language environment";

  config = mkIf cfg.enable {
    home.packages = [ pkgs.unison-ucm ];
    programs.neovim.plugins = [ pkgs.vimPlugins.vim-unison ];
  };
}
