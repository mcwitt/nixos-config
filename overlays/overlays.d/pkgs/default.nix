self: super:
let inherit (self) callPackage;
in
{
  awscli2 = callPackage ./awscli2 { };

  mcwitt = {
    gitignore = callPackage ./gitignore.nix { };
    scripts = callPackage ./scripts { };
  };
}
