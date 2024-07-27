{
  programs.emacs.init.usePackage = {

    emacs.extraConfig = ''
      :custom
      ;; TAB cycle if there are only few candidates
      ;; (completion-cycle-threshold 3)

      ;; Enable indentation+completion using the TAB key.
      ;; `completion-at-point' is often bound to M-TAB.
      (tab-always-indent 'complete)

      ;; Emacs 30 and newer: Disable Ispell completion function. As an alternative,
      ;; try `cape-dict'.
      (text-mode-ispell-word-completion nil)

      ;; Emacs 28 and newer: Hide commands in M-x which do not apply to the current
      ;; mode.  Corfu commands are hidden, since they are not used via M-x. This
      ;; setting is useful beyond Corfu.
      (read-extended-command-predicate #'command-completion-default-include-p))
    '';

    corfu = {
      enable = true;
      init = ''
        (global-corfu-mode)
      '';
      extraConfig = ''
        :custom
        (corfu-auto t)
        (corfu-auto-prefix 1)
        (corfu-separator ?\s)

        ;; (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
        ;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
        ;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
        ;; (corfu-preview-current nil)    ;; Disable current candidate preview
        ;; (corfu-preselect 'prompt)      ;; Preselect the prompt
        ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches
        ;; (corfu-scroll-margin 5)        ;; Use scroll margin
      '';
    };

    nerd-icons-corfu = {
      enable = true;
      config = ''
        (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter)
      '';
    };
  };
}
