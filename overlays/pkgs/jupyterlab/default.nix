{ fetchFromGitHub, directory ? "./jupyterlab" }:
let
  src = fetchFromGitHub {
    owner = "tweag";
    repo = "jupyterWith";
    rev = "7a6716f0c0a5538691a2f71a9f12b066bce7d55c";
    sha256 = "0q8g93if8k53ick13hrpzwbbc0ymkx87mzpa7dhkfyilj7vksa2g";
  };

  # nixpkgs revision used by jupyterWith
  pkgs = import "${src}/nix" {
    config = {
      allowUnfree = true; # needed for MKL
      allowBroken = true; # ihaskell is marked broken
    };
  };

  jupyter = import src { pkgs = pkgs; };

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
