{ config, lib, pkgs, ... }:
with lib;
{
  programs.emacs.init.usePackage = {

    counsel = {
      enable = true;
      demand = true;
      diminish = [ "counsel-mode" ];
      bind = { "C-x M-f" = "counsel-fzf"; };
      config = "(counsel-mode 1)";
    };

    counsel-projectile = {
      enable = true;
      after = [ "projectile" ];
      config = "(counsel-projectile-mode 1)";
    };

    counsel-tramp = {
      enable = true;
      bind = { "C-c f" = "counsel-tramp"; };
    };

    ivy = {
      enable = true;
      demand = true;
      diminish = [ "ivy-mode" ];
      bind = { "C-c C-r" = "ivy-resume"; };
      config = "(ivy-mode 1)";
    };

    ivy-hydra = {
      enable = true;
      after = [ "ivy" "hydra" ];
    };

    ivy-bibtex = {
      enable = true;
      bind = { "C-c r" = "ivy-bibtex"; };
    };

    ivy-pass = {
      enable = true;
      bind = { "C-c w" = "ivy-pass"; };
    };

    org-ref.config = ''
      (setq org-ref-completion-library 'org-ref-ivy-cite)
    '';

    projectile.config = ''
      (setq projectile-completion-system 'ivy)
    '';

    swiper = {
      enable = true;
      bind = { "C-s" = "swiper-isearch"; };
    };
  };
}
