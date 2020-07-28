{ pkgs, jupyter }:
let myHaskellPackages = import ../haskell-packages.nix { };
in jupyter.kernels.iHaskellWith {
  name = "haskell";
  packages = myHaskellPackages;
}
