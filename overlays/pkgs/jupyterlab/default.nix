{ mypkgs, directory ? "./jupyterlab" }:
let
  src = mypkgs.sources.jupyterWith;

  # nixpkgs revision used by jupyterWith
  pkgs = import "${src}/nix" {
    config = {
      allowUnfree = true; # needed for MKL
      allowBroken = true; # ihaskell is marked broken
    };
  };

  jupyter = import src { inherit pkgs; };

  ipython = import ./ipython.nix { inherit pkgs jupyter; };
  ihaskell = import ./ihaskell.nix { inherit pkgs jupyter; };

in jupyter.jupyterlabWith {
  kernels = [ ipython ihaskell ];
  inherit directory;
} // {
  # Inherit 'pkgs' so downstream derivations can access the nixpkgs
  # revision used by jupyterWith
  inherit pkgs;
}
