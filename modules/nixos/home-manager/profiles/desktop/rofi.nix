{
  config,
  inputs,
  lib,
  pkgs,
  ...
}:
{
  config = lib.mkIf config.profiles.desktop.enable {

    programs.rofi = {
      enable = true;

      # Stylix doesn't use correct syntax "<name> <size>"
      font =
        let
          inherit (config.stylix) fonts;
        in
        lib.mkForce "${fonts.monospace.name} ${toString fonts.sizes.desktop}";

      # This installs a copy of rofi without plugins but still using
      # global config, leading to "missing plugin" startup errors. Work
      # around by overriding rofi-pass to use rofi.finalPackage

      # pass.enable = true;

      plugins = with pkgs; [
        rofi-calc
        rofi-emoji
      ];

      # NOTE: can remove "start" subcommand after https://github.com/wez/wezterm/commit/e8886752e87c3240df4148828797459776abfa7f
      terminal = "${pkgs.wezterm}/bin/wezterm start";

      theme = {
        window = {
          width = "33%";
          border-radius = "6px";
        };
        element-icon = {
          size = "1em";
          margin = "0 0.25em 0 0";
        };
      };
    };

    # https://github.com/carnager/rofi-pass/issues/226
    xdg.configFile."rofi-pass/config".text = ''
      help_color="${config.lib.stylix.colors.withHashtag.base0D}"
    '';
  };
}
