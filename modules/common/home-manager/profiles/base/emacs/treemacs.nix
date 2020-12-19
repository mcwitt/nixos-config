{ config, lib, pkgs, ... }:
with lib;
let usePackageCfg = config.programs.emacs.init.usePackage;
in
{
  programs.emacs.init.usePackage = {
    treemacs = {
      enable = true;
      bind = {
        "M-0" = "treemacs-select-window";
        "C-x t 1" = "treemacs-delete-other-windows";
        "C-x t t" = "treemacs";
        "C-x t B" = "treemacs-bookmark";
        "C-x t C-t" = "treemacs-find-file";
        "C-x t M-t" = "treemacs-find-tag";
      };
      config =
        ''(setq treemacs-python-executable "${pkgs.python3}/bin/python")'';
    };

    treemacs-projectile = {
      enable = usePackageCfg.projectile.enable;
      after = [ "treemacs" "projectile" ];
    };

    treemacs-magit = {
      enable = usePackageCfg.magit.enable;
      after = [ "treemacs" "magit" ];
    };
  };
}
