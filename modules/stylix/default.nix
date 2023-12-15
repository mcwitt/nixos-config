{ inputs, pkgs, ... }:
{
  stylix = {
    base16Scheme = "${pkgs.base16-schemes}/share/themes/nord.yaml";
    # polarity = "dark"; # only relevant when stylix.base16Scheme not specified

    cursor.size = 48;

    image =
      let
        carinaNebula = builtins.fetchurl {
          url = "https://stsci-opo.org/STScI-01GA6KKWG229B16K4Q38CH3BXS.png";
          sha256 = "sha256:1a9lg35i4ipb7msp65gwlgf9al85idfvb8zfmgh7dwd39xvbd7z8";
        };
        nord-valley = "${inputs.nordic-wallpapers}/wallpapers/nord_valley.png";
      in
      nord-valley;

    fonts = {
      monospace = {
        package = pkgs.nerdifyFont pkgs.iosevka-comfy.comfy;
        name = "IosevkaComfy Nerd Font";
      };

      sizes = {
        desktop = 10;
        applications = 10;
      };
    };
  };
}
