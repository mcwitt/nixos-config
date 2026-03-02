{
  config,
  lib,
  pkgs,
  ...
}:
with lib;
let
  cfg = config.languages.html;
in
{
  options.languages.html.enable = mkEnableOption "Html language environment";

  config = mkIf cfg.enable {
    home.packages = [ pkgs.vscode-langservers-extracted ];
    programs.emacs.init.usePackage.eglot.hook = [ "(mhtml-mode . eglot-ensure)" ];
  };
}
