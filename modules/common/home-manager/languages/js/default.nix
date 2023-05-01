{ config, lib, ... }:
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
      js2-mode.enable = true;
      typescript-mode.enable = true;
      web-mode.enable = true;
    };
  };
}
