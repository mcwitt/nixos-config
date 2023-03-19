{ inputs, pkgs, ... }:
let
  backgrounds = {
    carinaNebula = builtins.fetchurl {
      url = "https://stsci-opo.org/STScI-01GA6KKWG229B16K4Q38CH3BXS.png";
      sha256 = "sha256:1a9lg35i4ipb7msp65gwlgf9al85idfvb8zfmgh7dwd39xvbd7z8";
    };
  };
in
{
  imports = [ inputs.stylix.nixosModules.stylix ];

  stylix.image = backgrounds.carinaNebula;

  stylix.polarity = "dark";

  stylix.fonts.monospace = {
    package = pkgs.iosevka-comfy.comfy;
    name = "Iosevka Comfy";
  };
}
