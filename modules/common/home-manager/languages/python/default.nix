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
          (ps: with ps; [
            black
            flake8
            mypy
            setuptools
            virtualenv
          ]);
      in
      [
        pkgs.nodePackages.pyright
        pythonEnv
      ];


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
        config = ''
          (add-to-list 'company-backends
                       '(company-anaconda :with company-capf))
        '';
      };

      dap-python.enable = true;

      lsp-pyright = {
        enable = true;
        hook = [
          ''
            (python-mode . (lambda ()
                              (require 'lsp-pyright)
                              (lsp-deferred)))
          ''
        ];
      };

      python-mode = {
        enable = true;
        mode = [ ''"\\.py\\'"'' ];
      };

      pyvenv.enable = true;
    };
  };
}
