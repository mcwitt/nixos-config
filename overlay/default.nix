{ inputs }:
final: prev:
let
  inherit (final) system;
  pkgsUnstable = inputs.nixpkgs-unstable.legacyPackages.${system};
in
{
  inherit (pkgsUnstable) wezterm;

  lib = prev.lib.extend (import ../lib.nix { inherit inputs; });

  nerdifyFont = final.callPackage ./nerdify-font.nix { };
}
