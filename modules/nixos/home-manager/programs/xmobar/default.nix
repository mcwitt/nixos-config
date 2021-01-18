{ config, lib, pkgs, ... }:
with lib;
let cfg = config.programs.xmobar;
in
{
  options.programs.xmobar =
    {
      enable = mkEnableOption "xmobar status bar";

      commands = mkOption
        {
          type = with types; listOf str;
          description = ''
            List of commands.
          '';
          default = [ ];
          example =
            [
              ''Run Date "<fc=#93a1a1>%F (%a) %T</fc>" "date" 10''
              ''Run StdinReader''
            ];
        };

      config = mkOption {
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

  config = mkIf cfg.enable
    (mkMerge [
      {
        home.packages = [ pkgs.xmobar ];
      }

      (mkIf (cfg.config != { }) {
        xdg.configFile."xmobar/xmobarrc".text =
          let
            valueToString = v:
              if isBool v then (if v then "True" else "False")
              else v;
            keyValues = concatStringsSep ", "
              (mapAttrsToList
                (k: v: ''
                  ${k} = ${valueToString v}
                '')
                cfg.config);
          in
          ''
            Config {
              ${keyValues}
            , commands = [
              ${concatStringsSep "  , " cfg.commands}
              ]
            }
          '';
      })
    ]);
}
