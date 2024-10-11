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
        pkgs.isort
        pkgs.pyright
        pythonEnv
      ];

    programs.emacs.overrides = _: _: { python = null; }; # use built-in python.el

    programs.emacs.init.usePackage = {

      code-cells = {
        hook = [ "(python-ts-mode . code-cells-mode-maybe)" ];
        config = ''
          (add-to-list 'code-cells-eval-region-commands '(python-ts-mode . python-shell-send-region))
        '';
      };

      eglot.hook = [ "(python-ts-mode . eglot-ensure)" ];

      ligature.config = ''
        (ligature-set-ligatures 'python-ts-mode '("->" "==" ">=" "<="))
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
        init = ''
          (add-to-list 'major-mode-remap-alist '(python-mode . python-ts-mode))
        '';
      };

      superword.hook = [ "(python-ts-mode . superword-mode)" ];
    };

    programs.git.ignores = pkgs.gitignores "Python";

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
