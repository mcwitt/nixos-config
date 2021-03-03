{ config, lib, pkgs, ... }:
with lib;
let
  cfg = config.languages.haskell;
  sources = import ../../../../../nix/sources.nix;
in
{
  options.languages.haskell = {
    enable = mkEnableOption "Haskell language environment";

    extraPackages = mkOption {
      default = self: [ ];
      type = hm.types.selectorFunction;
      defaultText = "hpkgs: []";
      example = literalExample "hpkgs: [ hpkgs.aeson hpkgs.lens ]";
      description = ''
        Packages to install globally.
      '';
    };

    hoogle.enable = mkEnableOption
      "Install a local hoogle with docs for packages in extraPackages.";
  };

  config = mkIf cfg.enable {
    home.packages =
      let
        ghcVersion = "8.10.4";
        ghcVersionStr = "ghc" + builtins.replaceStrings [ "." ] [ "" ] ghcVersion;
        ghcWithPackages' = with pkgs.haskell.packages.${ghcVersionStr};
          if cfg.hoogle.enable then ghcWithHoogle else ghcWithPackages;
        ghcEnv = ghcWithPackages' cfg.extraPackages;
        hls = import sources.all-hls { version = "1.0.0"; ghc = ghcVersion; };
      in
      [ ghcEnv ] ++ (with pkgs.haskellPackages;
      [
        brittany
        cabal-fmt
        cabal-install
        ghcEnv
        ghcid
        hls
        hlint
        ormolu
        stack
      ]);

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
          ''
            (haskell-mode . (lambda ()
                              (setq-local format-all-formatters '(("Haskell" ormolu)))))
          ''
        ];
        bindLocal.haskell-mode-map = {
          "C-c C-h" = "haskell-hoogle-lookup-from-local";
        };
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
        mode = [ ''("\\.cabal\\'" . haskell-cabal-mode)'' ];
      };

      fira-code-mode.hook = [ "haskell-mode" ];

      flycheck-haskell = {
        enable = true;
        hook = [ "(haskell-mode . flycheck-haskell-setup)" ];
      };

      lsp-haskell = {
        enable = true;
        hook = [ "(haskell-mode . lsp-deferred)" ];
      };

      ob-haskell = {
        enable = true;
        after = [ "org" ];
      };

      reformatter = {
        enable = true;
        hook = [ "(haskell-cabal-mode . cabal-fmt-on-save-mode)" ];
        config = ''
          (reformatter-define cabal-fmt
              :program "cabal-fmt"
              :lighter " CabalFmt")
        '';
      };
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
