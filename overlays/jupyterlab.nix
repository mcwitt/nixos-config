self: super:
let
  jupyterWithRepo = builtins.fetchGit {
    url = "https://github.com/tweag/jupyterWith";
    rev = "7a6716f0c0a5538691a2f71a9f12b066bce7d55c";
  };

  jupyter = import jupyterWithRepo { config.allowUnfree = true; };

  # import nixpkgs revision used by jupyterWith
  pkgs = import "${jupyterWithRepo}/nix/nixpkgs.nix" { };

  iPython = jupyter.kernels.iPythonWith {
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

  iHaskell = jupyter.kernels.iHaskellWith {
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
    kernels = [ iPython iHaskell ];
    directory = jupyter.mkDirectoryWith {
      extensions = [ "jupyterlab_vim" "jupyterlab-ihaskell" ];
    };
  };

in {
  jupyterlabEnv = pkgs.python3.buildEnv.override {
    extraLibs = [ jupyterlab pkgs.python3Packages.jupyter_client ];
  };
}
