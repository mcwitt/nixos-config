{ config, lib, pkgs, ... }:
with lib;
let
  cfg = config.programs.jupyterlab;

  mkIPython = ks: ks.iPythonWith {
    name = "python";
    packages = ps: with ps; [ pandas httpx seaborn ];
  };

  mkIHaskell = ks: ks.iHaskellWith {
    extraIHaskellFlags = "--codemirror Haskell"; # for jupyterlab syntax highlighting
    name = "haskell";
    packages = p: with p; [ hvega formatting ];
  };

in
{
  options.programs.jupyterlab = {
    enable = mkEnableOption "JupyterLab notebook server";

    kernels = mkOption {
      type = with types; listOf
        (functionTo
          (submodule {
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
          }));

      default = [ mkIPython mkIHaskell ];
    };
  };

  config =
    let jupyterEnvironment = pkgs.jupyterWith.jupyterlabWith {
      kernels = map (mk: mk pkgs.jupyterWith.kernels) cfg.kernels;
    }; in
    mkIf cfg.enable {
      home.packages = [ jupyterEnvironment ];
    };
}
