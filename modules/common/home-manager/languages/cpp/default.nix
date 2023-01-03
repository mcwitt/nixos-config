{ lib, config, pkgs, ... }:
with lib;
let cfg = config.languages.cpp;
in
{
  options.languages.cpp.enable = mkEnableOption "C/C++ language environment";

  config = mkIf cfg.enable {

    programs.emacs.init.usePackage = {

      cmake-mode = {
        enable = true;
        mode = [
          ''"\\.cmake\\'"''
          ''"CMakeLists.txt\\'"''
        ];
      };
    };

    programs.vscode.extensions = [
      pkgs.vscode-extensions.llvm-vs-code-extensions.vscode-clangd
    ];
  };
}
