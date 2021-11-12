{ lib, config, pkgs, ... }:
with lib;
let cfg = config.languages.cpp;
in
{
  options.languages.cpp.enable = mkEnableOption "C/C++ language environment";

  config = mkIf cfg.enable {

    home.packages = [ pkgs.clang-tools ];

    programs.emacs.init.usePackage = {
      lsp-clangd = {
        enable = true;
        hook = [
          "(c-mode . lsp-deferred)"
          "(c++-mode . lsp-deferred)"
        ];
      };

      cuda-mode = {
        enable = true;
        mode = [ ''"\\.cuh?\\'"'' ];
      };
    };

    programs.vscode.extensions = [
      pkgs.vscode-extensions.llvm-vs-code-extensions.vscode-clangd
    ];
  };
}
