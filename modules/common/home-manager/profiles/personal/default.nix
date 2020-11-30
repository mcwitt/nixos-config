{ config, lib, pkgs, ... }:
with lib;
let cfg = config.profiles.personal;
in
{
  options.profiles.personal.enable =
    mkEnableOption "Profile for use on machines I own";

  config = mkIf cfg.enable {

    home.packages = with pkgs; [ graphviz nixops ];

    languages = {
      R.enable = true;
      dhall.enable = true;
      haskell.enable = true;
      idris.enable = true;
      js.enable = true;
      nix.enable = true;
      python.enable = true;
      rust.enable = true;
      scala.enable = true;
      shell.enable = true;
    };

    tools = {
      aws.enable = true;
      kubernetes.enable = true;
    };
  };
}
