{ config, lib, pkgs, ... }:
with lib;
let cfg = config.profiles.personal;
in
{
  options.profiles.personal.enable =
    mkEnableOption "Profile for use on machines I own";

  config =
    let enableAll = keys: listToAttrs (map (key: nameValuePair key { enable = true; }) keys);
    in

    mkIf cfg.enable {
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
      } // enableAll [
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
