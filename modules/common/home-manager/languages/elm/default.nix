{ config, lib, pkgs, ... }:
with lib;
let cfg = config.languages.elm;
in
{
  options.languages.elm.enable = mkEnableOption "Elm language environment";

  config = mkIf cfg.enable {

    programs.emacs.init.usePackage = {
      elm-mode = {
        enable = true;
        mode = [ ''"\\.elm\\'"'' ];
      };

      subword.hook = [ "(elm-mode . subword-mode)" ];
    };

    programs.vscode.extensions = [ pkgs.vscode-extensions.elmtooling.elm-ls-vscode ];
  };
}
