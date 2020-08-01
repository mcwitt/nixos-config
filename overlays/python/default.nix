self: super:
let
  myOverride = {
    packageOverrides = self: super:
      let inherit (self) callPackage;
      in { vulture = callPackage ./vulture.nix { }; };
  };
in {
  # NOTE: Need to add an override for each required python version
  python2 = super.python2.override myOverride;
  python3 = super.python3.override myOverride;
}
