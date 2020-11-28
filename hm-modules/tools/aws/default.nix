{ config, lib, pkgs, ... }:
with lib;
let cfg = config.tools.aws;
in
{
  options.tools.aws.enable = mkEnableOption "AWS command-line tools";

  config = mkIf cfg.enable {

    home.packages = [ pkgs.awscli2 ];

    programs.fish.interactiveShellInit = ''
      # https://github.com/aws/aws-cli/issues/1079
      complete --command ${pkgs.awscli2}/bin/aws --no-files \
        --arguments '(begin; set --local --export COMP_SHELL fish set --local --export COMP_LINE (commandline) ${pkgs.awscli2}/bin/aws_completer | sed \'s/ $//\'; end)'
    '';
  };
}
