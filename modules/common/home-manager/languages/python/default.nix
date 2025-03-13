{
  lib,
  config,
  pkgs,
  ...
}:
with lib;
let
  cfg = config.languages.python;
in
{
  options.languages.python = {
    enable = mkEnableOption "Python language environment";

    globalPackages = mkOption {
      default = _: [ ];
      type = hm.types.selectorFunction;
      defaultText = "pypkgs: []";
      example = literalExample "pypkgs: with pypkgs; [ pandas requests ]";
      description = ''
        Packages to install globally.
      '';
    };
  };

  config = mkIf cfg.enable {
    home.packages =
      let
        pythonEnv = pkgs.python3.withPackages cfg.globalPackages;
      in
      [
        pkgs.ruff
        pkgs.basedpyright
        pythonEnv
      ];

    programs.emacs.overrides = _: prev: {
      python = null; # use built-in package
    };

    programs.emacs.init.usePackage = {

      code-cells = {
        hook = [ "(python-ts-mode . code-cells-mode-maybe)" ];
        config = ''
          (add-to-list 'code-cells-eval-region-commands '(python-ts-mode . python-shell-send-region))
        '';
      };

      eglot.hook = [ "(python-ts-mode . eglot-ensure)" ];

      format-all.hook = [ "(python-ts-mode . (lambda () (format-all-mode -1)))" ];

      ligature.config = ''
        (ligature-set-ligatures 'python-ts-mode '("->" "==" ">=" "<="))
      '';

      # Use python.el, NOT python-mode.el
      python = {
        enable = true;
        init = ''
          (add-to-list 'major-mode-remap-alist '(python-mode . python-ts-mode))
        '';
      };

      reformatter = {
        enable = true;
        hook = [ "(python-ts-mode . ruff-format-on-save-mode)" ];
        command = [
          "ruff-format-on-save-mode"
          "ruff-fix-on-save-mode"
        ];
        config = ''
          (reformatter-define ruff-format
            :program "ruff"
            :args (list "format" "--stdin-filename" (or (buffer-file-name) input-file))
            :lighter " RuffFmt")

          (reformatter-define ruff-fix
            :program "ruff"
            :args (list "check" "--fix-only" "--stdin-filename" (or (buffer-file-name) input-file))
            :lighter " RuffFix")
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
