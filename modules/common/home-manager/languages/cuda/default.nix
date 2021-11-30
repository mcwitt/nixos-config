{ lib, config, pkgs, ... }:
with lib;
let cfg = config.languages.cuda;
in
{
  options.languages.cuda.enable = mkEnableOption "CUDA language environment";

  config = mkIf cfg.enable {

    programs.emacs.init.usePackage = {

      cuda-mode = {
        enable = true;
        mode = [ ''"\\.cuh?\\'"'' ];
      };

      lsp-clangd = {
        hook = [ "(cuda-mode . lsp-deferred)" ];
        config = ''
          (add-to-list 'lsp-language-id-configuration '(cuda-mode . "cpp"))
        '';
      };
    };

    programs.vscode = {

      extensions = [
        pkgs.mcwitt.vscode-extensions.NVIDIA.nsight-vscode-edition
      ];

      userSettings.files.associations = {
        "*.cu" = "cpp";
        "*.cuh" = "cpp";
      };
    };
  };
}
