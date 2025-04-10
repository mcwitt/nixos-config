{
  config,
  lib,
  pkgs,
  ...
}:
with lib;
let
  cfg = config.languages.typst;
in
{
  options.languages.typst.enable = mkEnableOption "Typst language environment";

  config = mkIf cfg.enable {
    programs.emacs.init.usePackage.typst-ts-mode.enable = true;
  };
}
