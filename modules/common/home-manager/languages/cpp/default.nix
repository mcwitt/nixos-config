{ lib, config, pkgs, ... }:
with lib;
let cfg = config.languages.cpp;
in
{
  options.languages.cpp.enable = mkEnableOption "C/C++ language environment";

  config = mkIf cfg.enable {

    home.packages = with pkgs; [
      bear
      clang-tools
      cmake-language-server
    ];

    programs.emacs.init.usePackage = {

      cmake-mode = {
        enable = true;
        mode = [
          ''"\\.cmake\\'"''
          ''"CMakeLists.txt\\'"''
        ];
      };

      lsp-clangd = {
        enable = true;
        hook = [
          "(c-mode . lsp-deferred)"
          "(c++-mode . lsp-deferred)"
        ];
      };

      lsp-cmake = {
        enable = true;
        hook = [ "(cmake-mode . lsp-deferred)" ];
      };
    };

    programs.vscode.extensions = [
      pkgs.vscode-extensions.llvm-vs-code-extensions.vscode-clangd
    ];
  };
}
