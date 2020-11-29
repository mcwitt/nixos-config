{ config, lib, pkgs, ... }:
with lib;
let cfg = config.languages.haskell;
in
{
  options.languages.haskell.enable =
    mkEnableOption "Haskell language environment";

  config = mkIf cfg.enable {
    home.packages = with pkgs.haskellPackages; [
      brittany
      cabal-bounds
      cabal-install
      ghcid
      hlint
      ormolu
      stack
    ];

    programs.vscode = {
      extensions = with pkgs.vscode-extensions; [
        haskell.haskell
        justusadam.language-haskell
      ];
      userSettings.languageServerHaskell = {
        hieVariant = "haskell-language-server";
        formattingProvider = "ormolu";
      };
    };

    programs.emacs.init.usePackage = {
      haskell-mode = {
        enable = true;
        hook = [
          "(haskell-mode . interactive-haskell-mode)"
          "(haskell-mode . lsp)"
        ];
        bindLocal = {
          haskell-mode-map."C-c C-h" = "haskell-hoogle-lookup-from-local";
          haskell-cabal-mode-map."C-c C-f" = "cabal-fmt-buffer";
        };
        config = ''
          (setq haskell-interactive-popup-errors nil)
          (setq-default flycheck-disabled-checkers '(haskell-stack-ghc))
        '';
      };

      fira-code-mode.hook = [ "haskell-mode" ];

      flycheck-haskell = {
        enable = true;
        hook = [ "(haskell-mode . flycheck-haskell-setup)" ];
      };

      lsp-haskell = {
        enable = true;
        config = ''(setq lsp-haskell-server-path "haskell-language-server")'';
      };

      ormolu = {
        enable = true;
        hook = [ "(haskell-mode . ormolu-format-on-save-mode)" ];
      };

      reformatter = {
        enable = true;
        config = ''
          (reformatter-define cabal-fmt
              :program "cabal-fmt"
              :lighter " CabalFmt")
        '';
      };
    };
  };
}
