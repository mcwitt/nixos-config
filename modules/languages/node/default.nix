{ config, lib, pkgs, ... }:
with lib;
let
  cfg = config.languages.node;
  prefix = "${config.home.homeDirectory}/.npm-global";
in
{
  options.languages.node.enable = mkEnableOption "Node language environment";

  config = mkIf cfg.enable {
    home.packages = with pkgs; [ nodejs nodePackages.prettier ];

    home.file.".npmrc".text = ''
      prefix=${prefix}
    '';

    home.sessionVariables = { PATH = "${prefix}/bin:$PATH"; };

    programs.emacs.init.usePackage.js2-mode.enable = true;
  };
}
