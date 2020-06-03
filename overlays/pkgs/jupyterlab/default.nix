{ fetchFromGitHub, directory ? "./jupyterlab" }:
let
  src = fetchFromGitHub {
    owner = "tweag";
    repo = "jupyterWith";
    rev = "247764735993e604c1748751a95b68ab3e3ee5eb";
    sha256 = "156gblkghjadjqc38vdvsm7i5aa74wkh8q71rm4gwyvcwi8c82fx";
  };

  # nixpkgs revision used by jupyterWith
  pkgs = import "${src}/nix" {
    config = {
      allowUnfree = true; # needed for MKL
      allowBroken = true; # ihaskell is marked broken
    };
  };

  jupyter = import src { inherit pkgs; };

  ipython = import ./ipython.nix { inherit pkgs jupyter; };
  ihaskell = import ./ihaskell { inherit pkgs jupyter; };

in jupyter.jupyterlabWith {
  kernels = [ ipython ihaskell ];
  inherit directory;
} // {
  # Inherit 'pkgs' so downstream derivations can access the nixpkgs
  # revision used by jupyterWith
  inherit pkgs;
}
