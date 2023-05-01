{ lib, config, pkgs, ... }:
with lib;
let cfg = config.languages.cpp;
in
{
  options.languages.cpp.enable = mkEnableOption "C/C++ language environment";

  config = mkIf cfg.enable {

    programs.emacs.init.usePackage = {

      eglot.hook = [
        "(c-mode . eglot-ensure)"
        "(c++-mode . eglot-ensure)"
      ];

      cmake-mode.enable = true;
    };

    programs.vscode.extensions = [
      pkgs.vscode-extensions.llvm-vs-code-extensions.vscode-clangd
    ];
  };
}
