{ config, lib, pkgs, ... }:
with lib;
let
  cfg = config.languages.js;
  prefix = "${config.home.homeDirectory}/.npm-global";
in
{
  options.languages.js.enable = mkEnableOption "JavaScript/TypeScript language environment";

  config = mkIf cfg.enable {

    home.file.".npmrc".text = ''
      prefix=${prefix}
    '';

    home.sessionPath = [ "${prefix}/bin" ];

    programs.emacs.init.usePackage = {

      js2-mode = {
        enable = true;
        mode = [ ''"\\.js\\'"'' ];
      };

      typescript-mode = {
        enable = true;
        mode = [ ''"\\.tsx?\\'"'' ];
        hook = [ "(typescript-mode . lsp-deferred)" ];
      };

      web-mode = {
        enable = true;
        mode = [
          ''"\\.html\\'"''
          ''"\\.jsx\\'"''
        ];
      };
    };
  };
}
