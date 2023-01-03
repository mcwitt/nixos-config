{ config, lib, ... }:
with lib;
let cfg = config.programs.xmobar.rc;
in
{
  options.programs.xmobar.rc =
    {
      commands = mkOption
        {
          type = with types; listOf str;
          description = ''
            List of commands.
          '';
          default = [
            ''Run StdinReader''
            ''Run Date "<fc=#93a1a1>%F (%a) %T</fc>" "date" 10''
            ''
              Run DynNetwork     [ "--template" , "<dev>: <tx>kB/s|<rx>kB/s"
                                 , "--Low"      , "1000"
                                 , "--High"     , "5000"
                                 , "--low"      , "#859900"
                                 , "--normal"   , "#b58900"
                                 , "--high"     , "#dc322f"
                                 ] 10
            ''
          ];
          example =
            [
              ''Run Date "<fc=#93a1a1>%F (%a) %T</fc>" "date" 10''
              ''Run StdinReader''
            ];
        };

      extraConfig = mkOption {
        type = with types;
          attrsOf (nullOr (either str (either bool int)));
        description = ''
          xmobar configuration as a set of attributes.
        '';
        default = { };
        example = {
          bgColor = ''"#002b36"'';
          fgColor = ''"#839496"'';
          position = "TopP 0 192";
          border = "BottomB";
          borderColor = ''"#839496"'';
        };
      };
    };

  config = {
    programs.xmobar.extraConfig =
      let
        valueToString = v:
          if isBool v then (if v then "True" else "False")
          else v;
        keyValues = concatStringsSep ", "
          (mapAttrsToList
            (k: v: ''
              ${k} = ${valueToString v}
            '')
            cfg.extraConfig);
      in
      ''
        Config {
          ${keyValues}
        , commands = [
          ${concatStringsSep "  , " cfg.commands}
          ]
        }
      '';
  };
}
