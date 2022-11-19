{ config, lib, pkgs, ... }:
with lib;
let cfg = config.programs.jupyterlab; in
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

      default = [
        (ks: ks.iPythonWith {
          name = "python";
          packages = ps: with ps; [ pandas httpx seaborn ];
        })

        (ks: ks.iHaskellWith {
          extraIHaskellFlags = "--codemirror Haskell"; # for jupyterlab syntax highlighting
          name = "haskell";
          packages = ps: with ps; [ hvega formatting ];
        })
      ];
    };
  };

  config =
    let
      jupyterEnvironment = pkgs.jupyterWith.jupyterlabWith {
        kernels = map (mk: mk pkgs.jupyterWith.kernels) cfg.kernels;
      };
    in
    mkIf cfg.enable {
      home.packages = [ jupyterEnvironment ];
    };
}
