{
  programs.emacs.init.usePackage = {

    emacs.init = ''
      ;; TAB cycle if there are only few candidates
      (setq completion-cycle-threshold 3)

      ;; Emacs 28: Hide commands in M-x which do not apply to the current mode.
      ;; Corfu commands are hidden, since they are not supposed to be used via M-x.
      (setq read-extended-command-predicate
            #'command-completion-default-include-p)

      ;; Enable indentation+completion using the TAB key.
      ;; `completion-at-point' is often bound to M-TAB.
      (setq tab-always-indent 'complete))
    '';

    cape = {
      enable = true;

      init = ''
        ;; Add `completion-at-point-functions', used by `completion-at-point'.
        ;; NOTE: The order matters!
        (add-to-list 'completion-at-point-functions #'cape-dabbrev)
        (add-to-list 'completion-at-point-functions #'cape-file)
        (add-to-list 'completion-at-point-functions #'cape-elisp-block)
        ;;(add-to-list 'completion-at-point-functions #'cape-history)
        ;;(add-to-list 'completion-at-point-functions #'cape-keyword)
        ;;(add-to-list 'completion-at-point-functions #'cape-tex)
        ;;(add-to-list 'completion-at-point-functions #'cape-sgml)
        ;;(add-to-list 'completion-at-point-functions #'cape-rfc1345)
        ;;(add-to-list 'completion-at-point-functions #'cape-abbrev)
        ;;(add-to-list 'completion-at-point-functions #'cape-dict)
        ;;(add-to-list 'completion-at-point-functions #'cape-symbol)
        ;;(add-to-list 'completion-at-point-functions #'cape-line)
      '';

      bind = {
        "C-c p p" = "completion-at-point"; # capf
        "C-c p t" = "complete-tag"; # etags
        "C-c p d" = "cape-dabbrev"; # or dabbrev-completion
        "C-c p h" = "cape-history";
        "C-c p f" = "cape-file";
        "C-c p k" = "cape-keyword";
        "C-c p s" = "cape-symbol";
        "C-c p a" = "cape-abbrev";
        "C-c p l" = "cape-line";
        "C-c p w" = "cape-dict";
        "C-c p \\\\" = "cape-tex";
        "C-c p &" = "cape-sgml";
        "C-c p r" = "cape-rfc1345";
      };
    };

    corfu = {
      enable = true;
      init = ''
        (customize-set-variable 'corfu-auto t)
        (global-corfu-mode)
      '';
    };
  };
}
