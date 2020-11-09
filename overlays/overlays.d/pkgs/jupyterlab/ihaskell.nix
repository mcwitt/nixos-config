{ pkgs, jupyter }:
let myHaskellPackages = import ../haskell-packages.nix { };
in
jupyter.kernels.iHaskellWith {
  name = "haskell";
  packages = ps: myHaskellPackages ps ++ (with ps; [ ihaskell-hvega ]);
}
