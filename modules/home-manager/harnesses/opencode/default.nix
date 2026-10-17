{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.harnesses;
in
{
  config = lib.mkIf cfg.enable {
    # The built-in system theme inherits the terminal's ANSI palette, which
    # Stylix already manages, and preserves its foreground/background colors.
    stylix.targets.opencode.enable = false;

    programs.opencode = {
      enable = true;
      enableMcpIntegration = true;
      inherit (cfg) skills;

      settings = {
        plugin = [
          "${pkgs.worktrunk.src}/dev/opencode-plugin.ts"
        ];

      };

      tui.theme = "system";
    };
  };
}
