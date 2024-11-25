{ lib, pkgs, ... }:
{
  stylix = {
    base16Scheme = lib.mkDefault {
      yaml = "${pkgs.base16-schemes}/share/themes/nord.yaml";
      use-ifd = "auto";
    };
    # polarity = "dark"; # only relevant when stylix.base16Scheme not specified

    cursor.size = 48;

    image =
      let
        carina-nebula = builtins.fetchurl {
          url = "https://stsci-opo.org/STScI-01GA6KKWG229B16K4Q38CH3BXS.png";
          sha256 = "sha256:1a9lg35i4ipb7msp65gwlgf9al85idfvb8zfmgh7dwd39xvbd7z8";
        };
        mountain-cabin = builtins.fetchurl {
          url = "https://raw.githubusercontent.com/mcwitt/art/b406c51aa6f41856b2e76b558eac8f1c8eb79512/mountain-cabin.png";
          sha256 = "sha256:0lc8177x7msa06k4s1v9slqm4ap5rc6gpc8d2ck3km4xx7bn3x9g";
        };
        ngc602 = builtins.fetchurl {
          url = "https://live.staticflickr.com/65535/54088897300_3e378b6a5f_o_d.png";
          sha256 = "sha256:19wd9bvgp5hp3j0nfq69yf1yvn72zv7g2s8lp2ak18pihxsijcg1";
        };
      in
      lib.mkDefault ngc602;

    imageScalingMode = "stretch";

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
        package = pkgs.nerdifyFont pkgs.iosevka-comfy.comfy-motion-duo;
        name = "IosevkaComfyMotionDuo Nerd Font";
      };

      sizes = {
        applications = 9;
        desktop = 9;
        popups = 9;
        terminal = 9;
      };
    };
  };
}
