{
  programs.emacs.init.prelude = ''
    (setq read-process-output-max (* 1024 1024)) ;; 1mb
  '';

  programs.emacs.init.usePackage = {

    dap-mode = {
      enable = true;
      after = [ "lsp-mode" ];
    };

    dap-mouse = {
      enable = true;
      command = [ "dap-tooltip-mode" ];
    };

    dap-ui = {
      enable = true;
      command = [ "dap-ui-mode" ];
    };

    lsp-diagnostics = {
      enable = true;
      after = [ "lsp-mode" ];
    };

    lsp-headerline = {
      enable = true;
      command = [ "lsp-headerline-breadcrumb-mode" ];
    };

    lsp-lens = {
      enable = true;
      command = [ "lsp-lens-mode" ];
    };

    lsp-mode = {
      enable = true;
      command = [ "lsp" "lsp-deferred" ];
      hook = [ "(lsp-mode . lsp-enable-which-key-integration)" ];
      init = ''
        (setq lsp-keymap-prefix "C-c l")
        (setq lsp-lens-enable t)
      '';
      config = ''
        (define-key lsp-mode-map (kbd "C-c l") lsp-command-map)
        (setq lsp-file-watch-threshold 100000)
      '';
    };

    lsp-modeline = {
      enable = true;
      command = [ "lsp-modeline-workspace-status-mode" ];
    };

    lsp-treemacs = {
      enable = true;
      command = [ "lsp-treemacs-errors-list" ];
    };

    lsp-ui = {
      enable = true;
      command = [ "lsp-ui-mode" ];
      config = ''
        (setq lsp-ui-doc-position 'at-point)
        (setq lsp-ui-doc-show-with-mouse nil)
      '';
    };
  };
}
