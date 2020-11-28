{ config, lib, pkgs, ... }:
with lib;
let cfg = config.shell;
in
{
  options.shell = {
    aliases = mkOption {
      default = { };
      type = types.attrs;
      example = { rm = "${pkgs.coreutils}/bn/rm -i"; };
      description = "Shell aliases";
    };
  };

  config = {
    programs.bash.shellAliases = cfg.aliases;
    programs.fish.shellAliases = cfg.aliases;
    programs.zsh.shellAliases = cfg.aliases;
  };
}
