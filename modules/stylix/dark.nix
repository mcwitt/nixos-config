{
  lib,
  pkgs,
  ...
}:
let
  artemis-ii-earth-moon = pkgs.fetchurl {
    name = "art002e009289.jpg";
    url = "https://images-assets.nasa.gov/image/art002e009289/art002e009289~orig.jpg";
    sha256 = "0xdzmxmf3aqr4wlvrzm79cbxcw9f0m5mhp1hmm2dyg5sh6jc9a1a";
  };
in
{
  # specialisation is a NixOS feature; nix-darwin has no equivalent, so this dark
  # variant is wired into makeNixosSystem only (not makeDarwinSystem).
  specialisation.dark.configuration.stylix = {
    # Colors taken from modus-vivendi-tinted emacs theme
    base16Scheme = lib.mkForce {
      base00 = "#0d0e1c"; # bg-main
      base01 = "#1d2235"; # bg-dim
      base02 = "#4a4f69"; # bg-active
      base03 = "#61647a"; # border
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
      base0F = "#db8b3f"; # rust
    };

    polarity = lib.mkForce "dark";
    image = lib.mkForce artemis-ii-earth-moon;
  };
}
