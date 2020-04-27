self: super:
let mypkgs = super.callPackage ../pkgs { };
in { inherit mypkgs; }
