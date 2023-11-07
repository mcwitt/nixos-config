{ config, lib, ... }:
with lib;
let cfg = config.profiles.personal;
in
{
  imports = [ ./email.nix ];

  options.profiles.personal.enable =
    mkEnableOption "Profile for use on machines I own";

  config = mkIf cfg.enable {
    languages = {

      haskell = {
        enable = true;
        hls.enable = true;
        hoogle.enable = true;
        ormolu.enable = true;
        globalPackages = ps: with ps; [
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
        globalPackages = ps: with ps; [
          httpx
          hypothesis
          matplotlib
          pandas
          toolz
        ];
      };
    } // lib.genAttrs [
      "R"
      "agda"
      # "coq"
      "cpp"
      "cuda"
      "dhall"
      "elm"
      "go"
      "graphviz"
      # "idris"
      "js"
      "markdown"
      "plantuml"
      "rust"
      "scala"
      "shell"
      "sql"
      "terraform"
      "tex"
      "unison"
    ]
      (_: { enable = true; });

    programs.git = {
      userName = "Matt Wittmann";
      userEmail = "mcwitt@gmail.com";
      signing = {
        key = "981FF1A67C955E2E";
        signByDefault = true;
      };
    };

    tools = {
      aws.enable = true;
      kubernetes.enable = true;
    };
  };
}
