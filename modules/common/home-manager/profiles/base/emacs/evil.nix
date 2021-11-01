{ config, lib, ... }:
with lib;
let usePackageCfg = config.programs.emacs.init.usePackage;
in
{
  programs.emacs.init.usePackage = {

    csv-mode.hook = [
      ''
        (csv-mode . (lambda ()
                      (evil-local-set-key 'normal
                                          (kbd "TAB")
                                          'csv-tab-command)))
      ''
    ];

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
      diminish = [ "evil-collection-unimpaired-mode" ];
    };

    evil-commentary = {
      enable = true;
      after = [ "evil" ];
      config = "(evil-commentary-mode)";
      diminish = [ "evil-commentary-mode" ];
    };

    evil-escape = {
      enable = true;
      after = [ "evil" ];
      diminish = [ "evil-escape-mode" ];
      init = ''(setq-default evil-escape-key-sequence "fd")'';
      config = "(evil-escape-mode)";
    };

    evil-org = {
      enable = true;
      after = [ "evil" "org" ];
      hook = [ "(org-mode . evil-org-mode)" ];
      init = ''
        ;; temporary workaround for https://github.com/Somelauw/evil-org-mode/issues/93
        (fset 'evil-redirect-digit-argument 'ignore)
      '';
      config = ''
        (require 'evil-org-agenda)
        (evil-org-agenda-set-keys)

        ;; temporary workaround for https://github.com/Somelauw/evil-org-mode/issues/93
        (add-to-list 'evil-digit-bound-motions 'evil-org-beginning-of-line)
        (evil-define-key 'motion 'evil-org-mode
            (kbd "0") 'evil-org-beginning-of-line)
      '';
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

    treemacs-evil = {
      enable = usePackageCfg.treemacs.enable;
      after = [ "treemacs" "evil" ];
    };
  };
}
