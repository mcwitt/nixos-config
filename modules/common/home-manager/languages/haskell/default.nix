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
      cabal-install
      ghcid
      haskell-language-server
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
        mode = [
          ''("\\.hs\\'" . haskell-mode)''
          ''("\\.lhs\\'" . haskell-literate-mode)''
        ];
        hook = [
          "(haskell-mode . interactive-haskell-mode)"
          "(haskell-mode . subword-mode)"
        ];
        bindLocal.haskell-mode-map."C-c C-h" = "haskell-hoogle-lookup-from-local";
        config = ''
          (setq haskell-interactive-popup-errors nil)
          (setq-default flycheck-disabled-checkers '(haskell-stack-ghc))
        '';
      };

      haskell-doc = {
        enable = true;
        command = [ "haskell-doc-current-info" ];
      };

      haskell-cabal = {
        enable = true;
        hook = [ "(haskell-cabal-mode . cabal-fmt-on-save-mode)" ];
      };

      fira-code-mode.hook = [ "haskell-mode" ];

      flycheck-haskell = {
        enable = true;
        hook = [ "(haskell-mode . flycheck-haskell-setup)" ];
      };

      lsp-haskell = {
        enable = true;
        hook = [
          ''
            (haskell-mode . (lambda ()
                              (direnv-update-environment)
                              (lsp)))
          ''
        ];
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
