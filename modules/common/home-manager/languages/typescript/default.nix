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
      typescript-mode = {
        enable = true;
        mode = [ ''"\\.tsx?\\'"'' ];
        hook = [ "(typescript-mode . lsp-deferred)" ];
      };
    };
  };
}
