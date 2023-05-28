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

    corfu = {
      enable = true;
      init = ''
        (customize-set-variable 'corfu-auto t)
        (global-corfu-mode)
      '';
    };
  };
}
