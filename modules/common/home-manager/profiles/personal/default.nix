{
  config,
  lib,
  pkgs,
  ...
}:
with lib;
let
  cfg = config.profiles.personal;
in
{
  imports = [
    ./aichat.nix
    ./email.nix
  ];

  options.profiles.personal.enable = mkEnableOption "Profile for use on machines I own";

  config = mkIf cfg.enable {

    home.packages = with pkgs; [ claude-code ];

    languages = {
      haskell = {
        enable = true;
        hoogle.enable = true;
        globalPackages =
          ps: with ps; [
            aeson
            array
            containers
            lens
            lens-aeson
            linear
            monad-loops
            monoidal-containers
            mtl
            optparse-generic
            parsec
            random-fu
            raw-strings-qq
            rvar
            safe
            split
            streaming
            tasty
            tasty-hunit
            text
            turtle
            vector
          ];
      };

      python = {
        enable = true;
        globalPackages =
          ps: with ps; [
            httpx
            hypothesis
            matplotlib
            notebook
            pandas
            scipy
            seaborn
            sympy
            toolz
          ];
      };
    }
    //
      lib.genAttrs
        [
          "agda"
          # "coq"
          "cpp"
          "cuda"
          "go"
          "graphviz-dot"
          "html"
          "markdown"
          "rust"
          "shell"
          "sql"
          "typst"
        ]
        (_: {
          enable = true;
        });

    programs.git = {
      settings = {
        user.name = "Matt Wittmann";
        user.email = "mcwitt@gmail.com";
        gitHub.user = "mcwitt";
      };

      signing = {
        key = "B19E6AC805563E38";
        signByDefault = true;
      };
    };

    programs.texlive = {
      enable = true;
      extraPackages = tpkgs: {
        inherit (pkgs.texlive)
          scheme-basic

          # minimal set needed for emacs org latex export
          dvisvgm
          dvipng
          wrapfig
          amsmath
          ulem
          hyperref
          capt-of
          ;
      };
    };

    tools = {
      # aws.enable = true;
      kubernetes.enable = true;
    };
  };
}
