{ config, lib, pkgs, ... }:
with lib;
let cfg = config.languages.elm;
in
{
  options.languages.elm.enable = mkEnableOption "Elm language environment";

  config = mkIf cfg.enable {
    home.packages = with pkgs.elmPackages; [
      elm
      elm-format
      elm-language-server
    ];

    programs.emacs.init.usePackage = {
      elm-mode = {
        enable = true;
        mode = [ ''"\\.elm\\'"'' ];
      };

      fira-code-mode.hook = [ "elm-mode" ];

      lsp-elm = {
        enable = true;
        hook = [ "(elm-mode . lsp-deferred)" ];
      };

      subword.hook = [ "(elm-mode . subword-mode)" ];
    };

    programs.vscode.extensions = [ pkgs.vscode-extensions.elmtooling.elm-ls-vscode ];
  };
}
