{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.languages.haskell;
in
{
  options.languages.haskell = {
    enable = lib.mkEnableOption "Haskell language environment";

    globalPackages = lib.mkOption {
      default = _: [ ];
      type = lib.hm.types.selectorFunction;
      defaultText = "hpkgs: []";
      example = lib.literalExample "hpkgs: [ hpkgs.aeson hpkgs.lens ]";
      description = ''
        Packages to install globally.
      '';
    };

    hoogle.enable = lib.mkEnableOption "Install a local hoogle with docs for packages in globalPackages.";
  };

  config = lib.mkIf cfg.enable {
    home.packages =
      let
        ghcWithPackages' =
          with pkgs.haskellPackages;
          if cfg.hoogle.enable then ghcWithHoogle else ghcWithPackages;
        ghcEnv = ghcWithPackages' cfg.globalPackages;
      in
      [
        ghcEnv
        pkgs.haskellPackages.cabal-fmt
        pkgs.cabal-install
        pkgs.haskell-language-server
        pkgs.hlint
        pkgs.ormolu
        pkgs.haskellPackages.weeder
      ];

    programs.emacs.init.usePackage = {

      eglot.hook = [ "(haskell-mode . eglot-ensure)" ];

      haskell-mode = {
        enable = true;
        bindLocal.haskell-mode-map = {
          "C-c C-h" = "haskell-hoogle-lookup-from-local";
        };
        hook = [ "(haskell-mode . interactive-haskell-mode)" ];
        config = ''
          (setq haskell-interactive-popup-errors nil)
        '';
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

      ormolu = {
        enable = true;
        hook = [ "(haskell-mode . ormolu-format-on-save-mode)" ];
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
      filetypes = [
        "haskell"
        "lhaskell"
      ];
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
