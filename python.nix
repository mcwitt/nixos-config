pkgs: pkgs.python3.withPackages (ps: with ps;
  [ ipython
    jupyter
    matplotlib
    numpy
    pandas
    pillow
    pymc3
    pytorch
    requests
    scikitlearn
    scipy
    seaborn
    statsmodels
    sympy
  ])
