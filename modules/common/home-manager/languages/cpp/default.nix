{ lib, config, pkgs, ... }:
with lib;
let cfg = config.languages.cpp;
in
{
  options.languages.cpp = {
    enable = mkEnableOption "C/C++ language environment";
    cuda.enable = mkEnableOption "CUDA tools";
  };

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

      cuda-mode = mkIf cfg.cuda.enable {
        enable = true;
        mode = [ ''"\\.cuh?\\'"'' ];
      };

      lsp-clangd = {
        enable = true;
        hook = [
          "(c-mode . lsp-deferred)"
          "(c++-mode . lsp-deferred)"
        ];
      };

      lsp-clangd.config = mkIf cfg.cuda.enable ''
        (add-to-list 'lsp-language-id-configuration '(cuda-mode . "cpp"))
      '';

      lsp-cmake = {
        enable = true;
        hook = [ "(cmake-mode . lsp-deferred)" ];
      };
    };

    programs.vscode.extensions = [
      pkgs.vscode-extensions.llvm-vs-code-extensions.vscode-clangd
    ] ++ lib.optional cfg.cuda.enable pkgs.mcwitt.vscode-extensions.NVIDIA.nsight-vscode-edition;
  };
}
