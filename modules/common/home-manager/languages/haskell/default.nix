{ config, lib, pkgs, ... }:
with lib;
let cfg = config.languages.haskell;
in
{
  options.languages.haskell = {
    enable = mkEnableOption "Haskell language environment";

    globalPackages = mkOption {
      default = _: [ ];
      type = hm.types.selectorFunction;
      defaultText = "hpkgs: []";
      example = literalExample "hpkgs: [ hpkgs.aeson hpkgs.lens ]";
      description = ''
        Packages to install globally.
      '';
    };

    hoogle.enable = mkEnableOption
      "Install a local hoogle with docs for packages in globalPackages.";
  };

  config = mkIf cfg.enable {
    home.packages =
      let
        ghcWithPackages' = with pkgs.haskellPackages; if cfg.hoogle.enable then ghcWithHoogle else ghcWithPackages;
        ghcEnv = ghcWithPackages' cfg.globalPackages;
      in
      [ ghcEnv ];

    programs.emacs.init.usePackage = {

      haskell = {
        enable = true;
        command = [ "interactive-haskell-mode" ];
        hook = [ "(haskell-mode . interactive-haskell-mode)" ];
      };

      haskell-mode = {
        enable = true;
        mode = [
          ''("\\.hs\\'" . haskell-mode)''
          ''("\\.lhs\\'" . haskell-literate-mode)''
        ];
        bindLocal.haskell-mode-map = {
          "C-c C-h" = "haskell-hoogle-lookup-from-local";
        };
        config = ''
          (setq haskell-interactive-popup-errors nil)
        '';
      };

      haskell-doc = {
        enable = true;
        command = [ "haskell-doc-current-info" ];
      };

      haskell-cabal = {
        enable = true;
        mode = [ ''("\\.cabal\\'" . haskell-cabal-mode)'' ];
      };

      format-all.hook = [ "(haskell-mode . (lambda () (format-all-mode -1)))" ];

      ligature.config = ''
        (ligature-set-ligatures 'haskell-mode '("++" ".." "::"
                                                "->" ">-" "=>" ">="
                                                "-<" "<-" "=<" "<="
                                                ">>=" ">=>" "=>>"
                                                "<<=" "<=<" "<<="
                                                "<>" "<$>" "<&>" "<*>" "<|>" "<+>"
                                                "$>" "*>" "|>"
                                                "<$" "<*" "<|"
                                                "==" "/=" "===" "=/="
                                                "||" "&&"
                                                ">>>" "^>>" ">>^"
                                                "<<<" "^<<" "<<^"
                                                "+++" "***" "|||" "&&&"
                                                ))
      '';

      ob-haskell = {
        enable = true;
        after = [ "org" ];
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

      subword.hook = [ "(haskell-mode . subword-mode)" ];
    };

    programs.neovim.coc.settings.languageserver.haskell = {
      command = "haskell-language-server-wrapper";
      args = [ "--lsp" ];
      rootPatterns = [
        "*.cabal"
        "stack.yaml"
        "cabal.project"
        "package.yaml"
        "hie.yaml"
      ];
      filetypes = [ "haskell" "lhaskell" ];
    };

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
  };
}
