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

      eglot = {
        hook = [ "(cuda-mode . eglot-ensure)" ];
        config = ''
          (add-to-list 'eglot-server-programs '(cuda-mode "clangd"))
        '';
      };

      yasnippet-snippets.package = ps: ps.yasnippet-snippets.overrideAttrs (oldAttrs: {
        patches =
          let
            cudaSnippets = pkgs.writeText "yasnippet-cuda-mode.patch" ''
              diff --git a/snippets/cuda-mode/.yas-parents b/snippets/cuda-mode/.yas-parents
              new file mode 100644
              index 0000000..7a0ada1
              --- /dev/null
              +++ b/snippets/cuda-mode/.yas-parents
              @@ -0,0 +1 @@
              +c++-mode
            '';
          in
          oldAttrs.patches or [ ] ++ [ cudaSnippets ];
      });
    };

    programs.vscode = {

      extensions = [
        pkgs.vscode-extensions.NVIDIA.nsight-vscode-edition
      ];

      userSettings.files.associations = {
        "*.cu" = "cpp";
        "*.cuh" = "cpp";
      };
    };
  };
}
