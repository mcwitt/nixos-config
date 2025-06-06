{
  lib,
  pkgs,
  ...
}:
{
  stylix = {
    # Colors taken from modus-operandi emacs theme
    base16Scheme = {
      base00 = "#ffffff"; # bg-main
      base01 = "#f0f0f0"; # bg-dim
      base02 = "#c4c4c4"; # bg-active
      base03 = "#9f9f9f"; # border
      base04 = "#595959"; # fg-dim
      base05 = "#000000"; # fg-main
      base06 = "#193668"; # fg-alt
      base07 = "#0000b0"; # blue-cooler
      base08 = "#a60000"; # red
      base09 = "#6f5500"; # yellow
      base0A = "#f3d000"; # bg-yellow-intense
      base0B = "#006800"; # green
      base0C = "#005e8b"; # cyan
      base0D = "#0031a9"; # blue
      base0E = "#8f0075"; # magenta-warmer
      base0F = "#8a290f"; # rust
    };

    polarity = "light";

    cursor = {
      package = pkgs.bibata-cursors;
      name = "Bibata-Modern-Ice";
      size = 32;
    };

    image =
      let
        # carina-nebula = builtins.fetchurl {
        #   url = "https://stsci-opo.org/STScI-01GA6KKWG229B16K4Q38CH3BXS.png";
        #   sha256 = "sha256:1a9lg35i4ipb7msp65gwlgf9al85idfvb8zfmgh7dwd39xvbd7z8";
        # };
        # mountain-cabin = builtins.fetchurl {
        #   url = "https://raw.githubusercontent.com/mcwitt/art/b406c51aa6f41856b2e76b558eac8f1c8eb79512/mountain-cabin.png";
        #   sha256 = "sha256:0lc8177x7msa06k4s1v9slqm4ap5rc6gpc8d2ck3km4xx7bn3x9g";
        # };
        # ngc602 = builtins.fetchurl {
        #   url = "https://live.staticflickr.com/65535/54088897300_3e378b6a5f_o_d.png";
        #   sha256 = "sha256:19wd9bvgp5hp3j0nfq69yf1yvn72zv7g2s8lp2ak18pihxsijcg1";
        # };
        canyonlands = builtins.fetchurl {
          url = "https://upload.wikimedia.org/wikipedia/commons/9/99/Green_River_Overlook_Ekker_Butte.jpg";
          sha256 = "sha256:0qwspj9yr2gbzjj26pvzkiw1l79qda32xbrdzfz3scpl3a6f3dyd";
        };
      in
      lib.mkDefault canyonlands;

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
        applications = 9;
        desktop = 9;
        popups = 9;
        terminal = 9;
      };
    };
  };

  specialisation.dark.configuration.stylix = {
    # Colors taken from modus-vivendi emacs theme
    base16Scheme = lib.mkForce {
      base00 = "#000000"; # bg-main
      base01 = "#1e1e1e"; # bg-dim
      base02 = "#535353"; # bg-active
      base03 = "#646464"; # border
      base04 = "#989898"; # fg-dim
      base05 = "#ffffff"; # fg-main
      base06 = "#c6daff"; # fg-alt
      base07 = "#00bcff"; # blue-cooler
      base08 = "#ff5f59"; # red
      base09 = "#d0bc00"; # yellow
      base0A = "#efef00"; # yellow-intense
      base0B = "#44bc44"; # green
      base0C = "#00d3d0"; # cyan
      base0D = "#2fafff"; # blue
      base0E = "#feacd0"; # magenta
      base0F = "#db7b5f"; # rust
    };

    polarity = lib.mkForce "dark";
  };
}
