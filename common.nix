{ config, lib, pkgs, ... }:
with lib;
let
  cfg = config.common;
in
{
  options.common.shellAliases = mkOption {
    default = { };
    type = types.attrs;
    example = { rm = "${pkgs.coreutils}/bn/rm -i"; };
    description = ''
      Cross-shell aliases
    '';
  };

  config = {
    programs.bash.shellAliases = cfg.shellAliases;
    programs.fish.shellAliases = cfg.shellAliases;
    programs.zsh.shellAliases = cfg.shellAliases;
  };
}
