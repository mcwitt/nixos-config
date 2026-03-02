{
  config,
  lib,
  pkgs,
  ...
}:
with lib;
let
  cfg = config.languages.typst;
in
{
  options.languages.typst.enable = mkEnableOption "Typst language environment";

  config = mkIf cfg.enable {

    home.packages = [
      pkgs.tinymist
      pkgs.typstyle
    ];

    programs.emacs.init.usePackage = {
      typst-ts-mode.enable = true;

      eglot = {
        enable = true;
        hook = [ "(typst-ts-mode . eglot-ensure)" ];
        config = ''
          (add-to-list 'eglot-server-programs
                       '(typst-ts-mode . ("tinymist" "lsp")))
        '';
      };

      reformatter = {
        enable = true;
        command = [ "typstyle-on-save-mode" ];
        config = ''
          (reformatter-define typstyle
            :program "typstyle"
            :args '("--line-width" "80" "--wrap-text")
            :lighter " Typstyle")
        '';
      };
    };
  };
}
