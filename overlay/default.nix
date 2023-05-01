{ inputs }: final: prev:
{
  nerdifyFont = final.callPackage ./nerdify-font.nix { };
}
