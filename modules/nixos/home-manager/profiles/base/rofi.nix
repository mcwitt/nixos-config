{ config, inputs, lib, pkgs, ... }:
{
  home.packages = [
    (pkgs.rofi-pass.override { rofi = config.programs.rofi.finalPackage; })
  ];

  programs.rofi = {
    enable = true;

    font = "Iosevka Comfy 10";

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

    theme = config.scheme inputs.base16-rofi;
  };

  # https://github.com/carnager/rofi-pass/issues/226
  xdg.configFile."rofi-pass/config".text = ''
    help_color="${config.scheme.withHashtag.base0D}"
  '';
}
