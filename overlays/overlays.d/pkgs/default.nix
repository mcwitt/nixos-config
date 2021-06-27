self: super:
let inherit (self) callPackage;
in
{
  mcwitt = {
    gitignore = callPackage ./gitignore.nix { };
    scripts = callPackage ./scripts { };
  };
}
