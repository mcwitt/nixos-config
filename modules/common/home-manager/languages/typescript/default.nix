{ config, lib, pkgs, ... }:
with lib;
let cfg = config.languages.typescript;
in
{
  options.languages.typescript.enable = mkEnableOption "TypeScript language environment";

  config = mkIf cfg.enable {
    home.packages = with pkgs.nodePackages; [
      typescript
      typescript-language-server
    ];

    programs.emacs.init.usePackage = {

      lsp-typescript = {
        enable = true;
        hook = [ "(typescript-mode . (lambda () (lsp-deferred)))" ];
      };

      typescript-mode = {
        enable = true;
        mode = [ ''"\\.tsx?\\'"'' ];
      };
    };
  };
}
