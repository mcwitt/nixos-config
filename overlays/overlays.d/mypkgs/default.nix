self: super:
let inherit (self) callPackage;
in
{
  mypkgs = {
    gitignore = callPackage ./gitignore.nix { };
    scripts = callPackage ./scripts { };
  };
}
