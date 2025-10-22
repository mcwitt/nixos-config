{ config, lib, ... }:
{
  config = lib.mkIf config.profiles.base.enable {
    programs.emacs.init.usePackage = {
      consult = {
        enable = true;
        command = [ "consult-ripgrep" ];

        bind = {
          "C-c M-x" = "consult-mode-command";
          "C-c h" = "consult-history";
          "C-c k" = "consult-kmacro";
          "C-c m" = "consult-man";
          "C-c i" = "consult-info";
          # ([remap Info-search] . consult-info)

          "C-x M-:" = "consult-complex-command"; # orig. repeat-complex-command
          "C-x b" = "consult-buffer"; # orig. switch-to-buffer
          "C-x 4 b" = "consult-buffer-other-window"; # orig. switch-to-buffer-other-window
          "C-x 5 b" = "consult-buffer-other-frame"; # orig. switch-to-buffer-other-frame
          "C-x r b" = "consult-bookmark"; # orig. bookmark-jump
          "C-x p b" = "consult-project-buffer"; # orig. project-switch-to-buffer
          "C-x C-r" = "consult-recent-file";

          "M-#" = "consult-register-load";
          "M-'" = "consult-register-store"; # orig. abbrev-prefix-mark (unrelated)
          "C-M-#" = "consult-register";

          "M-y" = "consult-yank-pop"; # orig. yank-pop

          "M-g a" = "consult-org-agenda";
          "M-g e" = "consult-compile-error";
          "M-g f" = "consult-flymake"; # Alternative: consult-flycheck
          "M-g g" = "consult-goto-line"; # orig. goto-line
          "M-g h" = "consult-org-heading";
          "M-g M-g" = "consult-goto-line"; # orig. goto-line
          "M-g o" = "consult-outline"; # Alternative: consult-org-heading
          "M-g m" = "consult-mark";
          "M-g k" = "consult-global-mark";
          "M-g i" = "consult-imenu";
          "M-g I" = "consult-imenu-multi";

          "M-s d" = "consult-find";
          "M-s D" = "consult-locate";
          "M-s g" = "consult-grep";
          "M-s G" = "consult-git-grep";
          "M-s r" = "consult-ripgrep";
          "M-s l" = "consult-line";
          "M-s L" = "consult-line-multi";
          "M-s k" = "consult-keep-lines";
          "M-s u" = "consult-focus-lines";

          "M-s e" = "consult-isearch-history";
        };

        bindLocal.isearch-mode-map = {
          "M-e" = "consult-isearch-history"; # orig. isearch-edit-string
          "M-s e" = "consult-isearch-history"; # orig. isearch-edit-string
          "M-s l" = "consult-line"; # needed by consult-line to detect isearch
          "M-s L" = "consult-line-multi"; # needed by consult-line to detect isearch
        };

        bindLocal.minibuffer-local-map = {
          "M-s" = "consult-history"; # orig. next-matching-history-element
          "M-r" = "consult-history"; # orig. previous-matching-history-element
        };

        hook = [ "(completion-list-mode . consult-preview-at-point-mode)" ];

        init = ''
          ;; Optionally configure the register formatting. This improves the register
          ;; preview for `consult-register', `consult-register-load',
          ;; `consult-register-store' and the Emacs built-ins.
          (setq register-preview-delay 0.5
                register-preview-function #'consult-register-format)

          ;; Optionally tweak the register preview window.
          ;; This adds thin lines, sorting and hides the mode line of the window.
          (advice-add #'register-preview :override #'consult-register-window)

          ;; Use Consult to select xref locations with preview
          (setq xref-show-xrefs-function #'consult-xref
                xref-show-definitions-function #'consult-xref)

          ;; Override project-find-regexp with consult-ripgrep
          (advice-add #'project-find-regexp :override #'consult-ripgrep)
        '';

        config = ''
          ;; For some commands and buffer sources it is useful to configure the
          ;; :preview-key on a per-command basis using the `consult-customize' macro.
          (consult-customize
           consult-theme :preview-key '(:debounce 0.2 any)
           consult-ripgrep consult-git-grep consult-grep
           consult-bookmark consult-recent-file consult-xref
           consult--source-bookmark consult--source-file-register
           consult--source-recent-file consult--source-project-recent-file
           ;; :preview-key "M-."
           :preview-key '(:debounce 0.4 any))

          ;; Optionally configure the narrowing key.
          ;; Both < and C-+ work reasonably well.
          (setq consult-narrow-key "<") ;; "C-+"

          ;; Optionally make narrowing help available in the minibuffer.
          ;; You may want to use `embark-prefix-help-command' or which-key instead.
          (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)
        '';
      };

      consult-eglot = {
        enable = true;
        after = [ "consult" ];
        bindLocal.eglot-mode-map = {
          "C-M-." = "consult-eglot-symbols";
        };
      };
    };
  };
}
