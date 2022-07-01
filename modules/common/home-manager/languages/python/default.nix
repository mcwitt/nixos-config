{ lib, config, pkgs, ... }:
with lib;
let cfg = config.languages.python;
in
{
  options.languages.python = {
    enable = mkEnableOption "Python language environment";

    globalPackages = mkOption {
      default = _: [ ];
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
      let pythonEnv = pkgs.python3.withPackages cfg.globalPackages;
      in
      [
        pkgs.black
        pkgs.pyright
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

      ligature.config = ''
        (ligature-set-ligatures 'python-mode '("->" "==" ">=" "<="))
      '';

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

      py-isort = {
        enable = true;
        command = [
          "py-isort-buffer"
          "py-isort-region"
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

    programs.git.ignores = pkgs.mcwitt.gitignore.ghGitIgnoreLines "Python";

    programs.jupyterlab.kernels = [
      (ks: ks.iPythonWith {
        name = "python";
        packages = cfg.globalPackages;
      })
    ];

    programs.neovim.plugins = [ pkgs.vimPlugins.coc-pyright ];

    programs.vscode = mkIf (!pkgs.stdenv.isDarwin) {
      extensions = with pkgs.vscode-extensions; [
        ms-python.python
        ms-python.vscode-pylance
      ];
      userSettings.python.languageServer = "Pylance";
    };
  };
}
