{
  lib,
  config,
  pkgs,
  ...
}:
with lib;
let
  cfg = config.languages.cpp;
in
{
  options.languages.cpp.enable = mkEnableOption "C/C++ language environment";

  config = mkIf cfg.enable {

    programs.emacs.init.usePackage = {

      eglot.hook = [
        "(c-ts-mode . eglot-ensure)"
        "(c++-ts-mode . eglot-ensure)"
      ];

      cc-mode = {
        enable = true;
        init = ''
          (add-to-list 'major-mode-remap-alist '(c-mode . c-ts-mode))
          (add-to-list 'major-mode-remap-alist '(c++-mode . c++-ts-mode))
          (add-to-list 'major-mode-remap-alist '(c-or-c++-mode . c-or-c++-ts-mode))
        '';
      };

      cmake-mode = {
        enable = true;
        init = ''
          (add-to-list 'major-mode-remap-alist '(cmake-mode . cmake-ts-mode))
        '';
      };
    };

    programs.vscode.extensions = [
      pkgs.vscode-extensions.llvm-vs-code-extensions.vscode-clangd
    ];
  };
}
