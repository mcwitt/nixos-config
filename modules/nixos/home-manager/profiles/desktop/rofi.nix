{
  config,
  lib,
  pkgs,
  ...
}:
{
  config = lib.mkIf config.profiles.desktop.enable {

    programs.rofi = {
      enable = true;

      plugins = with pkgs; [
        rofi-calc
        rofi-emoji
      ];

      terminal = "${pkgs.wezterm}/bin/wezterm";

      theme =
        let
          inherit (config.lib.formats.rasi) mkLiteral;
        in
        {
          window = {
            width = mkLiteral "80ch";
            padding = mkLiteral "0.25em";
          };

          element-icon = {
            size = mkLiteral "1em";
            margin = mkLiteral "0 0.25em 0 0";
          };

          inputbar.children = map mkLiteral [
            "prompt"
            "textbox-prompt-colon"
            "entry"
            "case-indicator"
          ];

          textbox-prompt-colon = {
            expand = false;
            str = ":";
            margin = mkLiteral "0 0.25em 0 0";
          };
        };
    };
  };
}
