{ config, lib, pkgs, ... }:
with lib;
let
  cfg = config.languages.js;
  prefix = "${config.home.homeDirectory}/.npm-global";
in
{
  options.languages.js.enable = mkEnableOption "JS language environment";

  config = mkIf cfg.enable {
    home.packages = with pkgs; [ nodejs nodePackages.prettier ];

    home.file.".npmrc".text = ''
      prefix=${prefix}
    '';

    home.sessionPath = [ "${prefix}/bin" ];

    programs.emacs.init.usePackage = {
      js2-mode = {
        enable = true;
        mode = [ ''"\\.js\\'"'' ];
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
