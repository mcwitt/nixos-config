{
  config,
  lib,
  pkgs,
  ...
}:
with lib;
let
  cfg = config.languages.shell;
in
{
  options.languages.shell.enable = mkEnableOption "Shell language environment";

  config = mkIf cfg.enable {
    home.packages = with pkgs; [
      shellcheck
      shfmt
    ];

    programs.emacs.init.usePackage.ob-shell = {
      enable = true;
      after = [ "org" ];
    };
  };
}
