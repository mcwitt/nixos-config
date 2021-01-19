{ config, lib, pkgs, ... }:
with lib;
let cfg = config.programs.thefuck;
in
{
  options.programs.thefuck = {
    enable = mkEnableOption "TheFuck shell command fixer";
    enableBashIntegration = mkEnableOption "bash integration";
    enableFishIntegration = mkEnableOption "fish integration";
    enableZshIntegration = mkEnableOption "zsh integration";
  };

  config = mkIf cfg.enable {
    home.packages = [ pkgs.thefuck ];

    programs.bash.initExtra = mkIf cfg.enableBashIntegration ''
      eval "$(${pkgs.thefuck}/bin/thefuck --alias)"
    '';

    programs.fish.shellInit = mkIf cfg.enableFishIntegration ''
      ${pkgs.thefuck}/bin/thefuck --alias | source
    '';

    programs.zsh.initExtra = mkIf cfg.enableZshIntegration ''
      eval $(${pkgs.thefuck}/bin/thefuck --alias)
    '';
  };
}
