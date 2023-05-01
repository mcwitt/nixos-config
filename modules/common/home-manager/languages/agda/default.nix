{ config, lib, ... }:
with lib;
let cfg = config.languages.agda;
in
{
  options.languages.agda.enable = mkEnableOption "Agda language environment";

  config = mkIf cfg.enable {

    programs.emacs.init.usePackage.agda2-mode = {
      enable = true;
      bindLocal.agda2-mode-map = {
        "M-." = "agda2-goto-definition-keyboard";
        "M-," = "agda2-go-back";
      };
      config = ''
        ;; Workaround for usage with direnv https://github.com/agda/agda/issues/5664
        (advice-add 'agda2-restart :before 'envrc--update)
      '';
    };
  };
}
