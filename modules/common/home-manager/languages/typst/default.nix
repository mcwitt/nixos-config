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

    home.packages = [ pkgs.typstfmt ];

    programs.emacs.init.usePackage = {
      typst-ts-mode.enable = true;

      reformatter = {
        enable = true;
        command = [ "typstfmt-on-save-mode" ];
        config = ''
          (reformatter-define typstfmt
            :program "typstfmt"
            :args nil
            :lighter " TypstFmt")
        '';
      };
    };
  };
}
