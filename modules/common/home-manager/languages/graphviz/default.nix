{ config, lib, pkgs, ... }:
with lib;
let cfg = config.languages.graphviz;
in
{
  options.languages.graphviz.enable =
    mkEnableOption "Graphviz Dot language environment";

  config = mkIf cfg.enable {

    home.packages = [ pkgs.graphviz ];

    programs.emacs.init.usePackage = {

      graphviz-dot-mode = {
        enable = true;
        mode = [
          ''"\\.dot\\'"''
          ''"\\.gv\\'"''
        ];
        init = ''
          (setq graphviz-dot-dot-program "${pkgs.graphviz}/bin/dot")
        '';
      };

      ob-dot = {
        enable = true;
        after = [ "org" ];
      };
    };
  };
}
