{ config, lib, pkgs, ... }:
with lib;
let cfg = config.languages.latex;
in
{
  options.languages.latex.enable =
    mkEnableOption "LaTeX language environment";

  config = mkIf cfg.enable {

    home.packages = [ pkgs.texlive.combined.scheme-full ];

    programs.emacs.init.usePackage = {

      latex = {
        enable = true;
        package = epkgs: epkgs.auctex;
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
