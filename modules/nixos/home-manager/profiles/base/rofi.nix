{ config, inputs, lib, pkgs, ... }:
{
  home.packages = [
    (pkgs.rofi-pass.override {
      rofi = config.programs.rofi.finalPackage;
    })
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

    terminal = "wezterm";

    theme = config.scheme inputs.base16-rofi;
  };

  # https://github.com/carnager/rofi-pass/issues/226
  xdg.configFile."rofi-pass/config".text = ''
    help_color="${config.scheme.withHashtag.base0D}"
  '';
}
