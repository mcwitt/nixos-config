{ lib, config, pkgs, ... }:
with lib;
let cfg = config.languages.python;
in
{
  options.languages.python = {
    enable = mkEnableOption "Python language environment";

    extraPackages = mkOption {
      default = self: [ ];
      type = hm.types.selectorFunction;
      defaultText = "pypkgs: []";
      example = literalExample "pypkgs: with pypkgs; [ black pandas requests ]";
      description = ''
        Packages to install globally.
      '';
    };
  };

  config = mkIf cfg.enable {
    home.packages =
      let
        pythonEnv = pkgs.python3.withPackages
          (ps:
            (with ps; [
              black
              flake8
              mypy
            ]) ++ cfg.extraPackages ps);
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

      pyvenv = {
        enable = true;
        command = [ "pyvenv-activate" ];
      };
    };

    programs.jupyterlab.kernels = [
      (ks: ks.iPythonWith {
        name = "python";
        packages = cfg.extraPackages;
      })
    ];

    programs.vscode = {
      extensions = with pkgs.vscode-extensions; [
        ms-python.python
        ms-python.vscode-pylance
      ];
      userSettings.python.languageServer = "Pylance";
    };
  };
}
