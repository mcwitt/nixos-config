{ config, lib, pkgs, ... }:
with lib;
let
  cfg = config.shell;
in
{
  options.shell = {
    enable = mkEnableOption "Common shell configuration";
    aliases = mkOption {
      default = { };
      type = types.attrs;
      example = { rm = "${pkgs.coreutils}/bn/rm -i"; };
      description = "Cross-shell aliases";
    };
  };

  config = mkIf cfg.enable {
    programs.bash.shellAliases = cfg.aliases;
    programs.fish.shellAliases = cfg.aliases;
    programs.zsh.shellAliases = cfg.aliases;
  };
}
