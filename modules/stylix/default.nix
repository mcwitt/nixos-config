{
  pkgs,
  ...
}:
{
  stylix = {
    # Colors taken from modus-operandi-tinted emacs theme
    base16Scheme = {
      base00 = "#fbf7f0"; # bg-main
      base01 = "#efe9dd"; # bg-dim
      base02 = "#c9b9b0"; # bg-active
      base03 = "#9f9690"; # border
      base04 = "#595959"; # fg-dim
      base05 = "#000000"; # fg-main
      base06 = "#193668"; # fg-alt
      base07 = "#0000b0"; # blue-cooler
      base08 = "#a60000"; # red
      base09 = "#6d5000"; # yellow
      base0A = "#f3d000"; # bg-yellow-intense
      base0B = "#006300"; # green
      base0C = "#00598b"; # cyan
      base0D = "#0031a9"; # blue
      base0E = "#8f0075"; # magenta-warmer
      base0F = "#8a290f"; # rust
    };

    polarity = "light";

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
