{ config, lib, pkgs, ... }:
with lib;
let cfg = config.languages.dhall;
in
{
  options.languages.dhall.enable = mkEnableOption "Dhall language environment";

  config = mkIf cfg.enable {

    programs.emacs.init.usePackage = {

      lsp-dhall = {
        enable = true;
        hook = [ "(dhall-mode . lsp-deferred)" ];
      };

      dhall-mode = {
        enable = true;
        mode = [ ''"\\.dhall\\'"'' ];
      };
    };

    programs.vscode.extensions = with pkgs.vscode-extensions.dhall; [
      dhall-lang
      vscode-dhall-lsp-server
    ];
  };
}
