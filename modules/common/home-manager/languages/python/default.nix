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

      eglot.hook = [ "(python-mode . eglot-ensure)" ];

      ligature.config = ''
        (ligature-set-ligatures 'python-mode '("->" "==" ">=" "<="))
      '';

      project.config = ''
        (dolist (marker '("setup.py" "setup.cfg" "pyproject.toml"))
          (add-to-list 'my/project-root-markers marker))
      '';

      py-isort = {
        enable = true;
        command = [ "py-isort-buffer" "py-isort-region" "py-isort-before-save" ];
        init = ''
          (add-to-list 'safe-local-eval-forms '(add-hook 'before-save-hook #'py-isort-before-save))
        '';
      };

      # Use python.el, NOT python-mode.el
      python = {
        enable = true;
        package = _: null; # use built-in package
        init = ''
          (add-to-list 'major-mode-remap-alist '(python-mode . python-ts-mode))
        '';
      };
    };

    programs.git.ignores = lib.gitignores "Python";

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
