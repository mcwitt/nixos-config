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

      cc-mode.enable = true;

      cmake-mode.enable = true;
    };

    programs.vscode.profiles.default.extensions = [
      pkgs.vscode-extensions.llvm-vs-code-extensions.vscode-clangd
    ];
  };
}
