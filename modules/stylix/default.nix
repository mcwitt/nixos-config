{
  pkgs,
  ...
}:
{
  stylix = {
    # Hand-authored Base16 projection of the ef-melissa-dark Emacs theme.
    # Keep syntax colors readable against bg-main rather than using the
    # theme's low-contrast border/background colors in foreground slots.
    base16Scheme = {
      base00 = "#352718"; # bg-main
      base01 = "#483426"; # bg-dim
      base02 = "#79665f"; # bg-active / selection
      base03 = "#90918a"; # fg-dim / comments
      base04 = "#ccaa70"; # fg-alt
      base05 = "#e8e4b1"; # fg-main
      base06 = "#f8efd8"; # fg-mode-line-active
      base07 = "#f8efd8"; # strongest foreground (Base16 compatibility)
      base08 = "#ff7f7f"; # red
      base09 = "#ffa21f"; # yellow-warmer / orange
      base0A = "#e4b53f"; # yellow
      base0B = "#6fd560"; # green
      base0C = "#6fcad0"; # cyan
      base0D = "#57aff6"; # blue
      base0E = "#f0aac5"; # magenta
      base0F = "#ff7f4f"; # red-warmer / rust
    };

    polarity = "dark";

    fonts = {
      monospace = {
        package = pkgs.nerdifyFont pkgs.iosevka-comfy.comfy;
        name = "IosevkaComfy Nerd Font";
      };

      sansSerif = {
        package = pkgs.nerdifyFont pkgs.iosevka-comfy.comfy-duo;
        name = "IosevkaComfyDuo Nerd Font";
      };

      serif = {
        package = pkgs.merriweather;
        name = "Merriweather";
      };

      sizes = {
        applications = 10;
        desktop = 10;
        popups = 10;
        terminal = 10;
      };
    };
  };
}
