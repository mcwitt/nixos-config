{ pkgs, ... }:
let
  jupyterWithRepo = builtins.fetchGit {
    url = "https://github.com/tweag/jupyterWith";
    rev = "7a6716f0c0a5538691a2f71a9f12b066bce7d55c";
  };

  # allowUnfree needed for MKL
  jupyter = import jupyterWithRepo { config.allowUnfree = true; };

  # nixpkgs revision used by jupyterWith
  pkgs = import "${jupyterWithRepo}/nix/nixpkgs.nix" { };

  ipython = jupyter.kernels.iPythonWith {
    name = "python";
    packages = ps:
      with ps; [
        altair
        beautifulsoup4
        matplotlib
        numpy
        pandas
        patsy
        psycopg2
        # pyro
        pillow
        # pytorch
        pyyaml
        requests
        scikitlearn
        scipy
        seaborn
        # spacy
        statsmodels
        # tensorflow-bin
      ];
  };

  ihaskell = jupyter.kernels.iHaskellWith {
    name = "haskell";
    packages = ps:
      with ps; [
        aeson
        array
        containers
        fgl
        formatting
        heaps
        hvega
        lens
        monad-loops
        mtl
        parsec
        postgresql-simple
        req
        split
        transformers
        vector
      ];
  };

  jupyterlab = jupyter.jupyterlabWith {
    kernels = [ ipython ihaskell ];
    directory = jupyter.mkDirectoryWith {
      extensions = [ "jupyterlab_vim" "jupyterlab-ihaskell" ];
    };
  };

  jupyterlabEnv =
    pkgs.python3.buildEnv.override { extraLibs = [ jupyterlab ]; };

in {
  home.packages = [ jupyterlabEnv ];
  programs.zsh.envExtra = jupyterlab.env.shellHook;
}
