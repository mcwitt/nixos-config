{ config, lib, pkgs, ... }:
with lib;
let cfg = config.languages.java;
in
{
  options.languages.java.enable = mkEnableOption "Java language environment";

  config = mkIf cfg.enable {
    programs.emacs.init.usePackage = {

      lsp-java = {
        enable = true;
        hook = [
          ''
            (java-mode . (lambda ()
                           (direnv-update-environment)
                           (lsp)))
          ''
        ];
      };
    };
  };
}
