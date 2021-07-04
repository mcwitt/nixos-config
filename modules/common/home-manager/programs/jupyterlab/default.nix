{ config, lib, pkgs, ... }:
with lib;
let
  cfg = config.programs.jupyterlab;

  ipython = pkgs.jupyterWith.kernels.iPythonWith {
    name = "python";
    packages = ps: with ps; [ pandas httpx seaborn ];
  };

  ihaskell = pkgs.jupyterWith.kernels.iHaskellWith {
    extraIHaskellFlags = "--codemirror Haskell"; # for jupyterlab syntax highlighting
    name = "haskell";
    packages = p: with p; [ hvega formatting ];
  };

in
{
  options.programs.jupyterlab = {
    enable = mkEnableOption "JupyterLab notebook server";

    kernels = mkOption {
      type = with types; listOf (submodule {
        options = {
          spec = mkOption {
            type = types.package;
          };
          runtimePackages = mkOption {
            type = with types; listOf package;
          };
          override = mkOption {
            type = types.function;
          };
          overrideDerivation = mkOption {
            type = types.function;
          };
        };
      });
      default = [ ipython ihaskell ];
    };
  };

  config =
    let jupyterEnvironment = pkgs.jupyterWith.jupyterlabWith {
      kernels = cfg.kernels;
    }; in
    mkIf cfg.enable {
      home.packages = [ jupyterEnvironment ];
    };
}
