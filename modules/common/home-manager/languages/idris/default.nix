{ config, lib, ... }:
with lib;
let cfg = config.languages.idris;
in
{
  options.languages.idris.enable = mkEnableOption "Idris language environment";

  config = mkIf cfg.enable {
    programs.emacs.init.usePackage.idris-mode.enable = true;
  };
}
