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

      ligature.config = ''
        (ligature-set-ligatures 'python-mode '("->" "==" ">=" "<="))
      '';

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
