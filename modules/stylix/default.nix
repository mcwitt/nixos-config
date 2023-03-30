{ inputs, pkgs, ... }:
{
  stylix = {
    base16Scheme = "${inputs.base16-schemes}/nord.yaml";

    image =
      let
        carinaNebula = builtins.fetchurl {
          url = "https://stsci-opo.org/STScI-01GA6KKWG229B16K4Q38CH3BXS.png";
          sha256 = "sha256:1a9lg35i4ipb7msp65gwlgf9al85idfvb8zfmgh7dwd39xvbd7z8";
        };
      in
      carinaNebula;

    # only relevant when stylix.base16Scheme not specified
    polarity = "dark";

    fonts = {
      monospace = {
        package = pkgs.iosevka-comfy.comfy;
        name = "Iosevka Comfy";
      };

      sizes = {
        desktop = 10;
        applications = 10;
      };
    };
  };
}
