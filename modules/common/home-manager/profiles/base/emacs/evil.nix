{ config, lib, ... }:
with lib;
let usePackageCfg = config.programs.emacs.init.usePackage;
in
{
  programs.emacs.init.usePackage = {
    evil = {
      enable = true;
      init = ''
        (setq evil-want-C-u-scroll t)
        (setq evil-want-integration t)
        (setq evil-want-keybinding nil)
        (setq evil-respect-visual-line-mode t)
        (setq evil-undo-system 'undo-tree)
      '';
      config = "(evil-mode)";
    };

    evil-collection = {
      enable = true;
      after = [ "evil" ];
      config = "(evil-collection-init)";
    };

    evil-escape = {
      enable = true;
      after = [ "evil" ];
      diminish = [ "evil-escape-mode" ];
      init = ''(setq-default evil-escape-key-sequence "fd")'';
      config = "(evil-escape-mode)";
    };

    evil-smartparens = {
      enable = true;
      after = [ "evil" "smartparens" ];
      hook = [ "(smartparens-enabled . evil-smartparens-mode)" ];
    };

    evil-surround = {
      enable = true;
      after = [ "evil" ];
      config = "(global-evil-surround-mode)";
    };

    evil-string-inflection.enable = usePackageCfg.string-inflection.enable;

    csv-mode.hook = [
      ''
        (csv-mode . (lambda ()
                      (evil-local-set-key 'normal
                                          (kbd "TAB")
                                          'csv-tab-command)))
      ''
    ];

    kubernetes-evil = {
      enable = usePackageCfg.kubernetes.enable;
      after = [ "kubernetes" "evil" ];
    };

    org-agenda.bindLocal.org-agenda-mode-map = {
      "j" = "evil-next-line";
      "k" = "evil-previous-line";
      "C-u" = "evil-scroll-page-up";
      "C-d" = "evil-scroll-page-down";
      "C-w h" = "evil-window-left";
      "C-w l" = "evil-window-right";
    };

    evil-org = {
      enable = true;
      after = [ "org" ];
      hook = [ "(org-mode . evil-org-mode)" ];
      config = ''
        (require 'evil-org-agenda)
        (evil-org-agenda-set-keys)
      '';
    };

    treemacs-evil = {
      enable = usePackageCfg.treemacs.enable;
      after = [ "treemacs" "evil" ];
    };
  };
}
