{ config, lib, ... }:
{
  config = lib.mkIf config.profiles.base.enable {
    programs.emacs.init.usePackage = {
      emacs.init = ''
        ;; Add prompt indicator to `completing-read-multiple'.
        (defun crm-indicator (args)
          (cons (concat "[CRM] " (car args)) (cdr args)))
        (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

        ;; Grow and shrink minibuffer
        ;;(setq resize-mini-windows t)

        ;; Do not allow the cursor in the minibuffer prompt
        (setq minibuffer-prompt-properties
              '(read-only t cursor-intangible t face minibuffer-prompt))
        (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

        ;; Enable recursive minibuffers
        (setq enable-recursive-minibuffers t)
      '';

      vertico = {
        enable = true;
        init = ''
          (setq vertico-cycle t)
        '';
        config = ''
          (vertico-mode)
        '';
      };
    };
  };
}
