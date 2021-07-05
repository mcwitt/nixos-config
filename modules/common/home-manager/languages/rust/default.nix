{ config, lib, pkgs, ... }:
with lib;
let cfg = config.languages.rust;
in
{
  options.languages.rust.enable = mkEnableOption "Rust language environment";

  config = mkIf cfg.enable {

    home.packages = [ pkgs.rustup ];

    programs.emacs.init.usePackage = {
      rust-mode = {
        enable = true;
        hook = [ "(rust-mode . lsp-deferred)" ];
      };
    };

    programs.vscode = {
      extensions = with pkgs.vscode-extensions; [
        matklad.rust-analyzer
      ];
    };
  };
}
