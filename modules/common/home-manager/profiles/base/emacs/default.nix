{ config, inputs, nurNoPkgs, pkgs, ... }: {

  imports = [
    nurNoPkgs.repos.rycee.hmModules.emacs-init
    ./org.nix
  ];

  programs.emacs.package = pkgs.emacsUnstable;

  programs.emacs.overrides = final: prev: {

    base16-theme = prev.base16-theme.overrideAttrs (old: {
      patchPhase = (old.patchPhase or "") + (
        let schemeFile = config.scheme.override { slug = "custom"; } inputs.base16-emacs;
        in ''
          cp ${schemeFile} build/base16-custom-theme.el
        ''
      );
    });

    copilot = final.trivialBuild {
      pname = "copilot";
      src = inputs.copilot-el;
      packageRequires = with final; [ dash editorconfig s ];
    };

    git-sync = final.trivialBuild {
      pname = "git-sync";
      src = inputs.git-sync-el;
    };
  };

  programs.emacs.init = {
    enable = true;
    recommendedGcSettings = true;

    earlyInit = ''
      (menu-bar-mode -1)
      (scroll-bar-mode -1)
      (tool-bar-mode -1)
      (tooltip-mode -1)

      (defun emoji-set-font (frame)
        "Adjust the font settings of FRAME so Emacs can display emoji properly."
        (if (eq system-type 'darwin)
            ;; For NS/Cocoa
            (set-fontset-font t 'symbol (font-spec :family "Apple Color Emoji") frame 'prepend)
          ;; For Linux
          (set-fontset-font t 'symbol (font-spec :family "JoyPixels") frame 'prepend)))
      (emoji-set-font nil)
      (add-hook 'after-make-frame-functions 'emoji-set-font)
    '';

    prelude = ''
      (setq custom-file "~/.emacs.d/custom.el")

      (setq user-full-name "Matt Wittmann"
            user-mail-address "mcwitt@gmail.com")

      ;; Create backup files in system temp directory
      (setq backup-directory-alist
            `((".*" . ,temporary-file-directory)))
      (setq auto-save-file-name-transforms
            `((".*" ,temporary-file-directory t)))

      (setq split-height-threshold 100)

      ;; Prompt for y/n instead of yes/no
      (defalias 'yes-or-no-p 'y-or-n-p)

      ;; Indent with spaces
      (setq-default indent-tabs-mode nil)

      ;; Highlight matching parens
      (show-paren-mode 1)

      ;; Don't insert a double space between sentences when filling
      (setq-default sentence-end-double-space nil)

      ;; Always show line and column numbers
      (line-number-mode)
      (column-number-mode)

      ;; Highlight end-of-line whitespace only in prog-mode
      (add-hook 'prog-mode-hook (lambda () (setq-local show-trailing-whitespace t)))

      ;; Open URLs with Chromium
      (setq browse-url-browser-function 'browse-url-chromium)

      ;; Make package archives unavailable (use Nix)
      (setq package-archives nil)
    '';
  };

  programs.emacs.init.usePackage = {

    all-the-icons = {
      enable = true;
      config = ''
        (setq all-the-icons-spacer " ")
      '';
    };

    all-the-icons-completion = {
      enable = true;
      config = ''
        (all-the-icons-completion-mode)
      '';
    };

    all-the-icons-dired = {
      enable = true;
      command = [ "all-the-icons-dired-mode" ];
      hook = [ "(dired-mode . all-the-icons-dired-mode)" ];
    };

    autorevert = {
      enable = true;
      diminish = [ "auto-revert-mode" ];
    };

    ace-window = {
      enable = true;
      extraConfig = ''
        :bind* (("C-c w" . ace-window)
                ("M-o" . ace-window))
      '';
    };

    avy = {
      enable = true;
      demand = true;
      bind = {
        "C-:" = "avy-goto-char";
        "M-g w" = "avy-goto-word-or-subword-1";
      };
    };

    base16-theme = {
      enable = true;
      config = ''
        (load-theme 'base16-custom t)
      '';
    };

    beacon = {
      enable = true;
      diminish = [ "beacon-mode" ];
      config = ''
        (setq beacon-color "#b58900")
        (beacon-mode 1)
      '';
    };

    browse-at-remote = {
      enable = true;
      command = [ "browse-at-remote" ];
      bind = { "C-c B" = "browse-at-remote"; };
    };

    company = {
      enable = true;
      diminish = [ "company-mode" ];
      command = [ "company-mode" "company-doc-buffer" "global-company-mode" ];
      bindLocal.company-active-map = {
        "jk" = "company-complete";
      };
      config = ''
        (global-company-mode)
      '';
    };

    company-box = {
      enable = true;
      diminish = [ "company-box-mode" ];
      hook = [ "(company-mode . company-box-mode)" ];
      config = ''
        (setq company-box-icons-alist 'company-box-icons-all-the-icons)
      '';
    };

    company-emoji = {
      enable = true;
      config = ''
        (add-to-list 'company-backends 'company-emoji)
      '';
    };

    company-restclient = {
      enable = true;
      after = [ "company" "restclient" ];
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
        "M-s e" = "consult-isearch-history";
      };

      bindLocal.isearch-mode-map = {
        "M-e" = "consult-isearch-history";
        "M-s e" = "consult-isearch-history";
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
        consult-theme :preview-key '(:debounce 0.2 any)
        consult-ripgrep consult-git-grep consult-grep
        consult-bookmark consult-recent-file consult-xref
        consult--source-bookmark consult--source-file-register
        consult--source-recent-file consult--source-project-recent-file
        ;; :preview-key (kbd "M-.")
        :preview-key '(:debounce 0.4 any))

        ;; Optionally configure the narrowing key.
        ;; Both < and C-+ work reasonably well.
        (setq consult-narrow-key "<") ;; (kbd "C-+")

        ;; Optionally make narrowing help available in the minibuffer.
        ;; You may want to use `embark-prefix-help-command' or which-key instead.
        ;; (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)

        ;; Optionally configure a function which returns the project root directory.
        ;; There are multiple reasonable alternatives to chose from.
        (setq consult-project-root-function
              (lambda ()
                (when-let (project (project-current))
                  (car (project-roots project)))))

        (setq consult-ripgrep-command (string-join '("rg" "--null" "--line-buffered" "--color=ansi"
                                                     "--max-columns=1000"  "--no-heading" "--line-number"
                                                     "--smart-case" "." "-e" "ARG" "OPTS") " "))
      '';
    };

    copilot = {
      enable = true;
      command = [ "copilot-login" "copilot-mode" ];
      bindLocal.copilot-mode-map = {
        "TAB" = "copilot-accept-completion";
      };
      config = ''
        (setq copilot-node-executable "${pkgs.nodejs-16_x}/bin/node")
      '';
    };

    csv-mode = {
      enable = true;
      mode = [ ''"\\.[Cc][Ss][Vv]\\'"'' ];
      hook = [ "(csv-mode . csv-align-mode)" ];
    };

    dockerfile-mode = {
      enable = true;
      mode = [ ''"Dockerfile\\'"'' ];
    };

    edit-indirect.enable = true;

    eglot.enable = true;

    eldoc = {
      enable = true;
      diminish = [ "eldoc-mode" ];
      command = [ "eldoc-mode" ];
    };

    elisp-format = {
      enable = true;
      command = [ "elisp-format-region" "elisp-format-buffer" ];
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

    envrc = {
      enable = true;
      demand = true;
      bindLocal.envrc-mode-map = {
        "C-c e" = "envrc-command-map";
      };
      config = ''
        (envrc-global-mode)
      '';
    };

    esup = {
      enable = true;
      command = [ "esup" ];
    };

    evil = {
      enable = true;
      init = ''
        (setq evil-want-integration t)
        (setq evil-want-keybinding nil)
        (setq evil-respect-visual-line-mode t)
        (setq evil-undo-system 'undo-tree)
      '';
      config = ''
        (evil-mode)
      '';
    };

    evil-collection = {
      enable = true;
      after = [ "evil" ];
      config = ''
        (evil-collection-init)
      '';
      diminish = [ "evil-collection-unimpaired-mode" ];
    };

    evil-commentary = {
      enable = true;
      after = [ "evil" ];
      config = ''
        (evil-commentary-mode)
      '';
      diminish = [ "evil-commentary-mode" ];
    };

    evil-escape = {
      enable = true;
      after = [ "evil" ];
      diminish = [ "evil-escape-mode" ];
      init = ''(setq-default evil-escape-key-sequence "fd")'';
      config = ''
        (evil-escape-mode)
      '';
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
      config = ''
        (global-evil-surround-mode)
      '';
    };

    evil-string-inflection.enable = true;

    expand-region = {
      enable = true;
      bind = { "C-=" = "er/expand-region"; };
    };

    flycheck = {
      enable = true;
      demand = true;
      diminish = [ "flycheck-mode" ];
      bind = {
        "M-n" = "flycheck-next-error";
        "M-p" = "flycheck-previous-error";
      };
      config = ''
        (setq flycheck-check-syntax-automatically '(save idle-change new-line))
        (setq flycheck-idle-change-delay 0.5)
        (setq flycheck-checker-error-threshold nil)
        (global-flycheck-mode t)
      '';
    };

    format-all = {
      enable = true;
      diminish = [ "format-all-mode" ];
      hook = [
        "(prog-mode . format-all-mode)"
        "(format-all-mode . format-all-ensure-formatter)"
      ];
    };

    forge = {
      enable = true;
      after = [ "magit" ];
      config = ''
        (setq forge-add-default-bindings nil)
      '';
    };

    frames-only-mode = {
      enable = true;
      config = ''
        ;; Prevent error when attempting to open in a new frame
        ;; Must run before enabling mode
        (add-to-list 'frames-only-mode-use-window-functions #'embark-act)

        (frames-only-mode 1)
      '';
    };

    gist.enable = true;

    hl-todo = {
      enable = true;
      hook = [ "(prog-mode . hl-todo-mode)" ];
    };

    imenu = {
      enable = true;
      bind = { "C-c i" = "imenu"; };
    };

    ispell = {
      enable = true;
      config = ''
        (setq ispell-program-name "aspell")
      '';
    };

    json-mode = {
      enable = true;
      mode = [ ''"\\.json\\'"'' ];
      hook = [ "(json-mode . flycheck-mode)" ];
    };

    kubernetes = {
      enable = true;
      command = [ "kubernetes-overview" ];
    };

    ligature = {
      enable = true;
      config = ''
        (global-ligature-mode t)
      '';
    };

    logview = {
      enable = true;
      command = [ "logview-mode" ];
      mode = [ ''("\\.log\\(?:\\.[0-9]+\\)?\\'" . logview-mode)'' ];
    };

    magit = {
      enable = true;
      bind = {
        "C-x g" = "magit-status";
        "C-x M-g" = "magit-dispatch";
        "C-c M-g" = "magit-file-dispatch";
      };
    };

    marginalia = {
      enable = true;
      bind."M-A" = "marginalia-cycle";
      bindLocal.minibuffer-local-map."M-A" = "marginalia-cycle";
      init = ''
        (marginalia-mode)
      '';
    };

    mixed-pitch = {
      enable = true;
      hook = [ "(text-mode . mixed-pitch-mode)" ];
      diminish = [ "mixed-pitch-mode" ];
    };

    multiple-cursors = {
      enable = true;
      bind = {
        "C-x | e" = "mc/edit-lines";
        "C-x | d" = "mc/mark-all-dwim";
      };
    };

    orderless = {
      enable = true;
      init = ''
        (setq completion-styles '(orderless)
              completion-category-defaults nil
              completion-category-overrides '((file (styles . (partial-completion)))))
      '';
    };

    pinentry = {
      enable = true;
      config = ''
        (pinentry-start)
      '';
    };

    org-agenda = {
      enable = true;
      bindLocal.org-agenda-mode-map = {
        "j" = "evil-next-line";
        "k" = "evil-previous-line";
        "C-u" = "evil-scroll-page-up";
        "C-d" = "evil-scroll-page-down";
        "C-w h" = "evil-window-left";
        "C-w l" = "evil-window-right";
      };
    };

    pdf-tools = {
      enable = true;
      mode = [ ''("\\.pdf\\'" . pdf-view-mode)'' ];
      hook = [ "(pdf-view-mode . (lambda () (linum-mode -1)))" ];
      config = ''
        (pdf-tools-install t t)
        (setq pdf-view-use-scaling t)
        (setq-default pdf-view-display-size 'fit-page)
      '';
    };

    # needed by Flycheck
    pkg-info = {
      enable = true;
      command = [ "pkg-info-version-info" ];
    };

    powerline = {
      enable = true;
      config = ''
        (powerline-center-evil-theme)
        (defadvice load-theme (after powerline-reset-after-load-theme activate)
          "Reset powerline after switching theme."
          (powerline-reset))
      '';
    };

    rainbow-delimiters = {
      enable = true;
      hook = [ "(prog-mode . rainbow-delimiters-mode)" ];
    };

    restclient = {
      enable = true;
      command = [ "restclient-mode" ];
    };

    savehist = {
      enable = true;
      config = ''
        (savehist-mode)
      '';
    };

    smartparens = {
      enable = true;
      diminish = [ "smartparens-mode" ];
    };

    subword = {
      enable = true;
      diminish = [ "subword-mode" ];
    };

    string-inflection = {
      enable = true;
      bind = { "C-c u" = "string-inflection-all-cycle"; };
    };

    tree-sitter = {
      enable = true;
      config = ''
        (global-tree-sitter-mode)
        (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)
      '';
    };

    tree-sitter-langs.enable = true;

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

    treemacs-evil = {
      enable = true;
      after = [ "treemacs" "evil" ];
    };

    treemacs-magit = {
      enable = true;
      after = [ "treemacs" "magit" ];
    };

    undo-tree = {
      enable = true;
      diminish = [ "undo-tree-mode" ];
      config = ''
        (global-undo-tree-mode)
      '';
    };

    vertico = {
      enable = true;
      init = ''
        (setq vertico-cycle t)
      '';
      config = ''
        (vertico-mode)
      '';
    };

    which-key = {
      enable = true;
      diminish = [ "which-key-mode" ];
      init = ''
        (setq which-key-separator " ")
        (setq which-key-prefix-prefix "+")
      '';
      config = ''
        (which-key-mode 1)
      '';
    };

    yaml-mode = {
      enable = true;
      mode = [ ''"\\.ya?ml\\'"'' ];
      hook = [ "(yaml-mode . (lambda () (mixed-pitch-mode -1)))" ];
    };

    yasnippet = {
      enable = true;
      diminish = [ "yas-minor-mode" ];
      config = ''
        (yas-global-mode 1)
      '';
    };

    yasnippet-snippets.enable = true;
  };
}
