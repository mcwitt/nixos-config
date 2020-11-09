{ pkgs, ... }:
let
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
{ home.packages = [ rEnv ]; }
