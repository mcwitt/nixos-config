{ config, lib, pkgs, ... }:
with lib;
let
  cfg = config.languages.R;
  rEnv = with pkgs;
    rWrapper.override {
      packages = with rPackages; [
        bsts
        dplyr
        forcats
        ggplot2
        knitr
        lme4
        purrr
        rmarkdown
        stringr
        tibble
        tidyr
        xts
      ];
    };
in
{
  options.languages.R.enable = mkEnableOption "R language environment";

  config = mkIf cfg.enable {
    home.packages = [ rEnv ];
    programs.emacs.init.usePackage.ess.enable = true;
  };
}
