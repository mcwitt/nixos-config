{
  programs.emacs.init.usePackage = {
    lsp-mode = {
      enable = true;
      command = [ "lsp" ];
      hook = [
        "(lsp-mode . lsp-enable-which-key-integration)"
        "(lsp-mode . lsp-lens-mode)"
      ];
      init = ''
        (setq lsp-keymap-prefix "C-c l")
      '';
      config = ''
        (setq lsp-file-watch-threshold 30000)
      '';
    };

    lsp-modeline = {
      enable = true;
      command = [ "lsp-modeline-workspace-status-mode " ];
    };

    lsp-ui = {
      enable = true;
      command = [ "lsp-ui-mode" ];
      config = ''
        (setq lsp-ui-doc-position 'at-point)
      '';
    };

    posframe.enable = true;

    dap-mode = {
      enable = true;
      hook = [ "(lsp-mode . dap-mode)" ];
      config = "(dap-auto-configure-mode 1)";
    };

    dap-mouse = {
      enable = true;
      command = [ "dap-tooltip-mode" ];
    };

    dap-ui = {
      enable = true;
      command = [ "dap-ui-mode" ];
      hook = [ "(lsp-mode . dap-ui-mode)" ];
    };

    lsp-headerline = {
      enable = true;
      command = [ "lsp-headerline-breadcrumb-mode" ];
    };

    lsp-ivy = {
      enable = true;
      command = [ "lsp-ivy-workspace-symbol" ];
    };

    lsp-treemacs = {
      enable = true;
      command = [ "lsp-treemacs-errors-list" ];
    };
  };
}
