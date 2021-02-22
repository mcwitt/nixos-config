{ config, lib, pkgs, ... }:
with lib;
let cfg = config.profiles.personal;
in
{
  options.profiles.personal.enable =
    mkEnableOption "Profile for use on machines I own";

  config = mkIf cfg.enable {

    languages = {
      R.enable = true;
      dhall.enable = true;
      elm.enable = true;
      go.enable = true;
      graphviz.enable = true;

      haskell = {
        enable = true;
        hoogle.enable = true;
        extraPackages = ps: with ps; [ aeson lens lens-aeson ];
      };

      idris.enable = true;
      js.enable = true;
      latex.enable = true;
      markdown.enable = true;
      plantuml.enable = true;
      python.enable = true;
      rust.enable = true;
      scala.enable = true;
      shell.enable = true;
      sql.enable = true;
      terraform.enable = true;
      typescript.enable = true;
    };

    programs.git = {
      userName = "Matt Wittmann";
      userEmail = "mcwitt@gmail.com";
      signing = {
        key = "C181215189C439A4";
        signByDefault = true;
      };
    };

    tools = {
      aws.enable = true;
      kubernetes.enable = true;
    };
  };
}
