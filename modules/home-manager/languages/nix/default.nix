{
  config,
  lib,
  pkgs,
  ...
}:
with lib;
let
  cfg = config.languages.nix;
in
{
  options.languages.nix.enable = mkEnableOption "Nix language environment";

  config = mkIf cfg.enable {
    home.packages = with pkgs; [
      cachix
      nil
      niv
      nix-info
      nix-prefetch-git
      nix-prefetch-github
      nixfmt-rfc-style
    ];

    programs.emacs.init.usePackage = {

      eglot = {
        hook = [
          "(nix-mode . eglot-ensure)"
          "(nix-ts-mode . eglot-ensure)"
        ];
        config = ''
          (add-to-list 'eglot-server-programs `(nix-ts-mode . ,(assoc-default 'nix-mode eglot-server-programs)))
        '';
      };

      format-all.config = ''
        (add-to-list 'language-id--definitions '("Nix" nix-mode nix-ts-mode))
      '';

      nix-mode = {
        enable = true;
        init = ''
          (add-to-list 'major-mode-remap-alist '(nix-mode . nix-ts-mode))
        '';
        bindLocal.nix-mode-map = {
          "C-c C-z" = "nix-repl-show";
        };
      };

      nix-ts-mode = {
        enable = true;
        hook = [ ''(nix-ts-mode . (lambda () (setq format-all-formatters '(("Nix" nixfmt)))))'' ];
        command = [ "nix-ts-mode" ];
        bindLocal.nix-ts-mode-map = {
          "C-c C-z" = "nix-repl-show";
        };
      };

      subword.hook = [ "(nix-mode . subword-mode)" ];
    };

    programs.neovim.plugins = [ pkgs.vimPlugins.vim-nix ];
  };
}
