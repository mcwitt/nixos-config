{
  config,
  lib,
  pkgs,
  ...
}:
with lib;
let
  cfg = config.languages.R;
  rEnv =
    with pkgs;
    rWrapper.override {
      packages = with rPackages; [
        knitr
        lme4
        rmarkdown
        tidyverse
        xts
      ];
    };
in
{
  options.languages.R.enable = mkEnableOption "R language environment";

  config = mkIf cfg.enable {
    home.packages = [ rEnv ];

    programs.emacs.init.usePackage = {

      ess = {
        enable = true;
        init = "(require 'ess-site)";
      };

      ob-R = {
        enable = true;
        after = [ "org" ];
      };
    };
  };
}
