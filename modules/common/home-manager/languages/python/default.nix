{ lib, config, pkgs, ... }:
with lib;
let cfg = config.languages.python;
in
{
  options.languages.python.enable = mkEnableOption "Python language environment";

  config = mkIf cfg.enable {
    home.packages = with pkgs; [ black mypy ] ++ (with python3Packages;
      [ flake8 virtualenv ]);

    programs.emacs.init.usePackage = {
      anaconda-mode = {
        enable = true;
        hook = [ "python-mode" "(python-mode . anaconda-eldoc-mode)" ];
      };

      company-anaconda = {
        enable = true;
        config = "(add-to-list 'company-backends 'company-anaconda)";
      };

      format-all.hook = [ "(python-mode . format-all-mode)" ];

      pyvenv.enable = true;
    };
  };
}
