{ lib, config, pkgs, ... }:
with lib;
let cfg = config.languages.python;
in
{
  options.languages.python.enable =
    mkEnableOption "Python language environment";

  config = mkIf cfg.enable {
    home.packages =
      let
        pythonEnv = pkgs.python38.withPackages
          (ps: with ps; [ black flake8 mypy virtualenv ]);
      in
      [ pythonEnv ];

    programs.emacs.init.usePackage = {
      anaconda-mode = {
        enable = true;
        hook = [
          "(python-mode . anaconda-mode)"
          "(python-mode . anaconda-eldoc-mode)"
        ];
      };

      company-anaconda = {
        enable = true;
        config = "(add-to-list 'company-backends 'company-anaconda)";
      };

      dap-python.enable = true;

      format-all.hook = [ "(python-mode . format-all-mode)" ];

      pyvenv.enable = true;
    };
  };
}
