{
  config,
  lib,
  pkgs,
  ...
}:
with lib;
let
  cfg = config.languages.graphviz-dot;
in
{
  options.languages.graphviz-dot.enable = mkEnableOption "GraphViz Dot language environment";

  config = mkIf cfg.enable {
    home.packages = with pkgs; [
      graphviz
    ];

    programs.emacs.init.usePackage = {

      graphviz-dot-mode.enable = true;

      org.config = ''
        (add-to-list 'org-babel-load-languages '(dot . t))
        (org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages)
      '';
    };
  };
}
