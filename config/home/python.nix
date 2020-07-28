{ pkgs, ... }:
let
  jupyterlab = pkgs.mypkgs.jupyterlab.override {
    directory = "${builtins.getEnv "HOME"}/.jupyterlab/";
  };
  # Use package set from 'jupyterlab.pkgs' for consistency with jupyterWith
  python3Env = jupyterlab.pkgs.python3.withPackages
    (ps: with ps; [ black flake8 mypy jupyterlab ]);
in {
  home.packages = [ python3Env ];

  # HACK set JUPYTER_PATH and JUPYTERLAB
  programs.fish.loginShellInit = jupyterlab.env.shellHook;

  # HACK set JUPYTER_PATH and JUPYTERLAB for Emacs service,
  # which is run inside 'pkgs.runtimeShell'
  programs.bash = {
    enable = true;
    profileExtra = jupyterlab.env.shellHook; # for Emacs service
  };
}
