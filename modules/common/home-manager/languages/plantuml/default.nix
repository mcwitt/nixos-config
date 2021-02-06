{ config, lib, pkgs, ... }:
with lib;
let cfg = config.languages.plantuml;
in
{
  options.languages.plantuml.enable = mkEnableOption "PlantUML language environment";
  config = mkIf cfg.enable {
    home.packages = with pkgs; [ adoptopenjdk-bin plantuml ];

    programs.emacs.init.usePackage = {

      org.config = ''
        (require 'ob-plantuml)
        (setq org-plantuml-exec-mode 'jar)
        (setq org-plantuml-jar-path "${pkgs.plantuml}/lib/plantuml.jar")
      '';

      plantuml-mode = {
        enable = true;
        mode = [ ''"\\.plantuml\\'"'' ];
        config = ''
          (setq plantuml-jar-path "${pkgs.plantuml}/lib/plantuml.jar"
                plantuml-default-exec-mode 'jar)
        '';
      };
    };
  };
}
