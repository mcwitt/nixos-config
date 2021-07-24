{ config, lib, pkgs, ... }:
with lib;
let cfg = config.profiles.personal;
in
{
  options.profiles.personal.enable =
    mkEnableOption "Profile for use on machines I own";

  config = mkIf cfg.enable {
    languages = {

      haskell = {
        enable = true;
        hoogle.enable = true;
        extraPackages = ps: with ps; [
          aeson
          array
          containers
          lens
          lens-aeson
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
          text
          turtle
          vector
        ];
      };

      python = {
        enable = true;
        extraPackages = ps: with ps; [
          httpx
          matplotlib
          pandas
          toolz
        ];
      };
    } // pkgs.mcwitt.lib.setAll { enable = true; } [
      "R"
      "coq"
      "dhall"
      "elm"
      "go"
      "graphviz"
      "idris"
      "js"
      "latex"
      "markdown"
      "plantuml"
      "rust"
      "scala"
      "shell"
      "sql"
      "terraform"
    ];

    programs.git = {
      userName = "Matt Wittmann";
      userEmail = "mcwitt@gmail.com";
      signing = {
        key = "C181215189C439A4";
        signByDefault = true;
      };
    };

    programs.jupyterlab.enable = true;

    tools = {
      aws.enable = true;
      kubernetes.enable = true;
    };
  };
}
