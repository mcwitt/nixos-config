{ config, lib, ... }:
with lib;
let cfg = config.languages.tex;
in
{
  options.languages.tex.enable =
    mkEnableOption "TeX language environment";

  config = mkIf cfg.enable {

    programs.emacs.init.usePackage = {

      tex = {
        enable = true;
        package = ps: ps.auctex;
        config = ''
          (add-to-list 'TeX-view-program-selection '(output-pdf "PDF Tools"))
        '';
      };

      ob-latex = {
        enable = true;
        after = [ "org" ];
      };
    };
  };
}
