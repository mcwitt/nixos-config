{ config, lib, pkgs, ... }:
with lib;
let cfg = config.languages.nix;
in
{
  options.languages.nix.enable = mkEnableOption "Nix language environment";

  config = mkIf cfg.enable {
    home.packages = with pkgs; [
      cachix
      niv
      nix-info
      nix-prefetch-git
      nix-prefetch-github
      nixpkgs-fmt
      rnix-lsp
    ];

    programs.emacs.init.usePackage = {

      eglot.hook = [ "(nix-mode . eglot-ensure)" ];

      nix-mode = {
        enable = true;
        mode = [ ''"\\.nix\\'"'' ];
        bindLocal.nix-mode-map = {
          "C-c C-z" = "nix-repl-show";
        };
      };

      nix-repl = {
        enable = true;
        command = [ "nix-repl" "nix-repl-show" ];
      };

      project.config = ''
        (add-to-list 'my/project-root-markers "flake.nix")
      '';

      subword.hook = [ "(nix-mode . subword-mode)" ];
    };

    programs.neovim.plugins = [ pkgs.vimPlugins.vim-nix ];
  };
}
