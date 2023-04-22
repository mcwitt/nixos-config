{ inputs }: final: prev:
{
  lib = prev.lib.extend (import ../lib.nix { inherit inputs; });
  nerdifyFont = final.callPackage ./nerdify-font.nix { };
}
