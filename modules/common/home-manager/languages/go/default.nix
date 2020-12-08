{ config, lib, pkgs, ... }:
with lib;
let cfg = config.languages.go;
in
{
  options.languages.go.enable = mkEnableOption "Go language environment";

  config = mkIf cfg.enable {
    home.packages = with pkgs; [ golint gopls ];
    programs.go.enable = true;

    programs.emacs.init.usePackage = {
      go-mode = {
        enable = true;
        mode = [ ''"\\.go\\'"'' ];
        hook = [ "(go-mode . lsp-deferred)" ];
      };

      dap-go.enable = true;
    };
  };
}
