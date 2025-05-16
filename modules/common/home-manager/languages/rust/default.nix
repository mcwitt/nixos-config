{
  config,
  lib,
  pkgs,
  ...
}:
with lib;
let
  cfg = config.languages.rust;
in
{
  options.languages.rust.enable = mkEnableOption "Rust language environment";

  config = mkIf cfg.enable {

    programs.emacs.init.usePackage.rust-mode.enable = true;

    programs.vscode.profiles.default.extensions = with pkgs.vscode-extensions; [
      rust-lang.rust-analyzer
    ];
  };
}
