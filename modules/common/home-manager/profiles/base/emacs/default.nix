{ config, inputs, lib, nurNoPkgs, pkgs, ... }: {

  imports = [
    nurNoPkgs.repos.rycee.hmModules.emacs-init
    ./completion
    ./jupyter.nix
    ./lsp.nix
    ./org.nix
    ./theme.nix
  ];

  home.packages = [ pkgs.emacs-all-the-icons-fonts ];

  programs.emacs.package = pkgs.emacsUnstable;

  programs.emacs.overrides = final: prev: {

    copilot =
      let src = inputs.copilot-el;
      in final.melpaBuild rec {
        pname = "copilot";
        version = "20230220.0";
        commit = src.rev;

        inherit src;

        packageRequires = with final; [ dash editorconfig s ];

        recipe = pkgs.writeText "recipe" ''
          (copilot
          :repo "zerolfx/copilot.el"
          :fetcher github
          :files ("dist" "*.el"))
        '';

        meta.description = "Emacs plugin for GitHub Copilot";
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

      ;; Always split horizontally (i.e. vertical stack)
      (setq split-width-threshold nil)

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

    all-the-icons-dired = {
      enable = true;
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

    beacon = {
      enable = true;
      diminish = [ "beacon-mode" ];
      config = ''
        (setq beacon-color "${config.lib.stylix.colors.withHashtag.orange}")
        (beacon-mode 1)
      '';
    };

    browse-at-remote = {
      enable = true;
      command = [ "browse-at-remote" ];
      bind = { "C-c B" = "browse-at-remote"; };
    };

    code-cells = {
      enable = true;
      bindLocal.code-cells-mode-map = { "C-c C-c" = "code-cells-eval"; };
      hook = lib.optional config.languages.python.enable "(python-mode . code-cells-mode-maybe)";
      config = ''
        ;; https://github.com/astoff/code-cells.el#speed-keys
        (let ((map code-cells-mode-map))
          (define-key map [remap evil-search-next] (code-cells-speed-key 'code-cells-forward-cell)) ;; n
          (define-key map [remap evil-paste-after] (code-cells-speed-key 'code-cells-backward-cell)) ;; p
          (define-key map [remap evil-backward-word-begin] (code-cells-speed-key 'code-cells-eval-above)) ;; b
          (define-key map [remap evil-forward-word-end] (code-cells-speed-key 'code-cells-eval)) ;; e
          (define-key map [remap evil-jump-forward] (code-cells-speed-key 'outline-cycle))) ;; TAB
      '';
    };

    command-log-mode = {
      enable = true;
      command = [ "command-log-mode" ];
    };

    copilot = {
      enable = false;
      command = [ "copilot-login" "copilot-mode" ];
      bind = {
        "C-TAB" = "copilot-accept-completion-by-word";
      };
      bindLocal.copilot-completion-map = {
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

    eldoc = {
      enable = true;
      package = _: null; # use built-in package
      diminish = [ "eldoc-mode" ];
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
        (setq evil-undo-system 'undo-fu)
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

    flymake = {
      enable = true;
      package = _: null; # use built-in package
      bind = {
        "M-n" = "flymake-goto-next-error";
        "M-p" = "flymake-goto-prev-error";
      };
    };

    flymake-shellcheck = {
      enable = true;
      hook = [
        ''
          (sh-mode . (lambda ()
                       (flymake-shellcheck-load)
                       (flymake-mode)))
        ''
      ];
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
    };

    frames-only-mode = {
      enable = true;
      config = lib.optionalString config.programs.emacs.init.usePackage.format-all.enable ''
        (add-to-list 'frames-only-mode-use-window-functions 'format-all--buffer-from-hook)
      '' + ''
        (frames-only-mode 1)
      '';
    };

    gist.enable = true;

    hl-todo = {
      enable = true;
      hook = [ "(prog-mode . hl-todo-mode)" ];
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
    };

    key-chord = {
      enable = true;
      config = ''
        ;; Escape insert mode with "fd"
        (setq key-chord-two-keys-delay 0.1)
        (key-chord-define evil-insert-state-map "fd" 'evil-normal-state)
        (key-chord-mode 1)
      '';
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

    magit-extras = {
      enable = true;
      demand = true; # e.g. to add entry to `project-switch-commands'
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

    pinentry = {
      enable = true;
      config = ''
        (pinentry-start)
      '';
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

    project = {
      enable = true;
      package = _: null; # use built-in package
      init = ''
        ;; Improved functionality for finding project root directories
        ;; Adapted from https://andreyorst.gitlab.io/posts/2022-07-16-project-el-enhancements/

        (defcustom my/project-root-markers
          nil
          "Files or directories that indicate the root of a project"
          :type '(repeat string)
          :group 'project)

        (defun my/project-root-p (dir)
          "Check if DIR has any of the project root markers."
          (catch 'found
            (dolist (marker my/project-root-markers)
              (when (file-expand-wildcards (concat dir marker))
                (throw 'found marker)))))

        (defun my/project-find-root (dir)
          (let ((root (locate-dominating-file dir #'my/project-root-p)))
            (and root (cons 'transient root))))
      '';

      config = ''
        (add-to-list 'project-find-functions #'my/project-find-root)
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

    undo-fu.enable = true;

    undo-fu-session = {
      enable = true;
      config = ''
        (undo-fu-session-global-mode)
      '';
    };

    vundo = {
      enable = true;
      bind = { "C-x u" = "vundo"; };
      bindLocal.vundo-mode-map = {
        "h" = "vundo-backward";
        "l" = "vundo-forward";
        "k" = "vundo-previous";
        "j" = "vundo-next";
      };
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
