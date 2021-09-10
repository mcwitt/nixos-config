{ config, lib, pkgs, ... }:
with lib;
{
  programs.emacs.init.usePackage = {

    vertico = {
      enable = true;
      init = ''
        (setq vertico-cycle t)
      '';
      config = ''
        (vertico-mode)
      '';
    };

    orderless = {
      enable = true;
      init = ''
        (setq completion-styles '(orderless)
              completion-category-defaults nil
              completion-category-overrides '((file (styles . (partial-completion)))))
      '';
    };

    savehist = {
      enable = true;
      config = ''
        (savehist-mode)
      '';
    };

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
      (setq enable-recursive-minibuffers t))
    '';

    marginalia = {
      enable = true;
      bind."M-A" = "marginalia-cycle";
      bindLocal.minibuffer-local-map."M-A" = "marginalia-cycle";
      config = ''
        (marginalia-mode)
      '';
    };

    consult = {
      enable = true;

      bind = {
        "C-c h" = "consult-history";
        "C-c m" = "consult-mode-command";
        "C-c b" = "consult-bookmark";
        "C-c k" = "consult-kmacro";
        "C-x M-:" = "consult-complex-command";
        "C-x b" = "consult-buffer";
        "C-x 4 b" = "consult-buffer-other-window";
        "C-x 5 b" = "consult-buffer-other-frame";
        "M-g M-g" = "consult-goto-line";
        "M-g o" = "consult-outline";
        "M-g m" = "consult-mark";
        "M-g k" = "consult-global-mark";
        "M-s f" = "consult-find";
        "M-s L" = "consult-locate";
        "M-s g" = "consult-grep";
        "M-s G" = "consult-git-grep";
        "M-s s" = "consult-ripgrep";
        "M-s l" = "consult-line";
        "M-s m" = "consult-multi-occur";
        "M-s k" = "consult-keep-lines";
        "M-s u" = "consult-focus-lines";
        "M-s e" = "consult-isearch";
      };

      bindLocal.isearch-mode-map = {
        "M-e" = "consult-isearch";
        "M-s e" = "consult-isearch";
        "M-s l" = "consult-line";
      };

      hook = [ "(completion-list-mode . consult-preview-at-point-mode)" ];

      init = ''
        ;; Optionally configure the register formatting. This improves the register
        ;; preview for `consult-register', `consult-register-load',
        ;; `consult-register-store' and the Emacs built-ins.
        (setq register-preview-delay 0
              register-preview-function #'consult-register-format)

        ;; Optionally tweak the register preview window.
        ;; This adds thin lines, sorting and hides the mode line of the window.
        (advice-add #'register-preview :override #'consult-register-window)

        ;; Use Consult to select xref locations with preview
        (setq xref-show-xrefs-function #'consult-xref
              xref-show-definitions-function #'consult-xref)
      '';

      config = ''
        ;; Optionally configure preview. The default value
        ;; is 'any, such that any key triggers the preview.
        ;; (setq consult-preview-key 'any)
        ;; (setq consult-preview-key (kbd "M-."))
        ;; (setq consult-preview-key (list (kbd "<S-down>") (kbd "<S-up>")))
        ;; For some commands and buffer sources it is useful to configure the
        ;; :preview-key on a per-command basis using the `consult-customize' macro.
        (consult-customize
         consult-ripgrep consult-git-grep consult-grep consult-bookmark consult-recent-file
         consult--source-file consult--source-project-file consult--source-bookmark
         :preview-key (kbd "M-."))

        ;; Optionally configure the narrowing key.
        ;; Both < and C-+ work reasonably well.
        (setq consult-narrow-key "<") ;; (kbd "C-+")

        ;; Optionally make narrowing help available in the minibuffer.
        ;; You may want to use `embark-prefix-help-command' or which-key instead.
        ;; (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)

        ;; Optionally configure a function which returns the project root directory.
        ;; There are multiple reasonable alternatives to chose from.
        ;;;; 1. project.el (project-roots)
        (setq consult-project-root-function
              (lambda ()
                (when-let (project (project-current))
                  (car (project-roots project)))))
        ;;;; 2. projectile.el (projectile-project-root)
        ;; (autoload 'projectile-project-root "projectile")
        ;; (setq consult-project-root-function #'projectile-project-root)
        ;;;; 3. vc.el (vc-root-dir)
        ;; (setq consult-project-root-function #'vc-root-dir)
        ;;;; 4. locate-dominating-file
        ;; (setq consult-project-root-function (lambda () (locate-dominating-file "." ".git")))

        (setq consult-ripgrep-command (string-join '("rg" "--null" "--line-buffered" "--color=ansi"
                                                     "--max-columns=1000"  "--no-heading" "--line-number"
                                                     "--smart-case" "." "-e" "ARG" "OPTS") " "))
      '';
    };

    consult-compile = {
      enable = true;
      bind = {
        "M-g e" = "consult-compile-error";
      };
    };

    consult-flymake = {
      enable = true;
      bind = {
        "M-g f" = "consult-flymake";
      };
    };

    consult-imenu = {
      enable = true;
      bind = {
        "M-g i" = "consult-imenu";
        "M-g I" = "consult-project-imenu";
      };
    };

    consult-org = {
      enable = true;
      after = [ "org" ];
    };

    consult-xref = {
      enable = true;
      command = [ "consult-xref" ];
    };

    embark = {
      enable = true;
      bind = {
        "C-." = "embark-act";
        "C-;" = "embark-dwim";
        "C-h B" = "embark-bindings";
      };
      init = ''
        ;; Optionally replace the key help with a completing-read interface
        (setq prefix-help-command #'embark-prefix-help-command)
      '';
      config = ''
        ;; Hide the mode line of the Embark live/completions buffers
        (add-to-list 'display-buffer-alist
                     '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                       nil
                       (window-parameters (mode-line-format . none))))
      '';
    };

    embark-consult = {
      enable = true;
      after = [ "embark" "consult" ];
      demand = true;
      hook = [ "(embark-collect-mode . consult-preview-at-point-mode)" ];
    };

    bibtex-actions = {
      enable = true;
      bind."C-c r" = "bibtex-actions-insert-citation";
      bindLocal.minibuffer-local-map."M-b" = "bibtex-actions-insert-preset";
      after = [ "org" "bibtex-completion" ];
      config = ''
        (require 'embark)

        ;; Make the 'bibtex-actions' bindings and targets available to `embark'.
        (add-to-list 'embark-target-finders 'bibtex-actions-citation-key-at-point)
        (add-to-list 'embark-keymap-alist '(bib-reference . bibtex-actions-map))
        (add-to-list 'embark-keymap-alist '(citation-key . bibtex-actions-buffer-map))
        (setq bibtex-actions-bibliography (list (concat org-notes-references-directory "master.bib")))
      '';
    };
  };
}
