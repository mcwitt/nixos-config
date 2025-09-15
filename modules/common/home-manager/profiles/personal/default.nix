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

    home.packages = [ pkgs.claude-code ];

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
          "html"
          "markdown"
          "rust"
          "shell"
          "typst"
        ]
        (_: {
          enable = true;
        });

    programs.git = {
      userName = "Matt Wittmann";
      userEmail = "mcwitt@gmail.com";
      signing = {
        key = "981FF1A67C955E2E";
        signByDefault = true;
      };
    };

    programs.texlive = {
      enable = true;
      extraPackages = tpkgs: { inherit (tpkgs) scheme-basic; };
    };

    tools = {
      aws.enable = true;
      kubernetes.enable = true;
    };
  };
}
