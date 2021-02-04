{ lib, config, pkgs, ... }:
with lib;
let cfg = config.languages.python;
in
{
  options.languages.python.enable =
    mkEnableOption "Python language environment";

  config = mkIf cfg.enable {
    home.packages =
      let
        pythonEnv = pkgs.python38.withPackages
          (ps: with ps; [ black flake8 mypy setuptools virtualenv ]);
      in
      [ pythonEnv ];


    programs.emacs = {
      overrides = self: super: {
        anaconda-mode = super.anaconda-mode.overrideAttrs (_: {
          src = pkgs.fetchFromGitHub
            {
              owner = "dakra";
              repo = "anaconda-mode";
              rev = "810163d5a65e62d58f363e2edaa3be70e6d82e25";
              sha256 = "13v478n35k71pz5sah807ckh641pb7hzffsjvdglsv4sc1nl61zq";
            };
        });
      };

      init.usePackage = {
        anaconda-mode = {
          enable = true;
          hook = [
            "(python-mode . anaconda-mode)"
            "(python-mode . anaconda-eldoc-mode)"
          ];
        };

        company-anaconda = {
          enable = true;
          config = ''
            (add-to-list 'company-backends
                         '(company-anaconda :with company-capf))
          '';
        };

        dap-python.enable = true;

        pyvenv.enable = true;
      };
    };
  };
}
