{ config, lib, ... }:
with lib;
let cfg = config.languages.js;
in
{
  config = mkIf cfg.enable {
    programs.chromium.extensions = let react-developer-tools = "fmkadmapgofadopljbjfkapdkoienihi"; in
      [ react-developer-tools ];
  };
}
