{ lib, config, pkgs, ... }:
with lib;
let cfg = config.languages.sql;
in
{
  options.languages.sql.enable = mkEnableOption "SQL language environment";

  config = mkIf cfg.enable {
    home.packages = with pkgs; [ pgcli pgformatter ];
  };
}
