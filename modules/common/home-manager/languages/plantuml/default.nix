{
  config,
  lib,
  pkgs,
  ...
}:
with lib;
let
  cfg = config.languages.plantuml;
in
{
  options.languages.plantuml.enable = mkEnableOption "PlantUML language environment";
  config = mkIf cfg.enable {
    home.packages = with pkgs; [
      openjdk
      plantuml
    ];

    programs.emacs.init.usePackage = {

      ob-plantuml = {
        enable = true;
        config = ''
          (setq org-plantuml-exec-mode 'jar)
          (setq org-plantuml-jar-path "${pkgs.plantuml}/lib/plantuml.jar")
        '';
      };

      plantuml-mode = {
        enable = true;
        config = ''
          (setq plantuml-jar-path "${pkgs.plantuml}/lib/plantuml.jar"
                plantuml-default-exec-mode 'jar)
        '';
      };
    };
  };
}
