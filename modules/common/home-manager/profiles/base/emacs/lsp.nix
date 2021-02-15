{
  programs.emacs.init.usePackage = {
    dap-mode = {
      enable = true;
      after = [ "lsp-mode" ];
    };

    lsp-ivy = {
      enable = true;
      command = [ "lsp-ivy-workspace-symbol" ];
    };

    lsp-mode = {
      enable = true;
      command = [ "lsp" "lsp-deferred" ];
      hook = [
        "(lsp-mode . lsp-enable-which-key-integration)"
        "(lsp-mode . lsp-lens-mode)"
      ];
      init = ''
        (setq lsp-keymap-prefix "C-c l")
      '';
      config = ''
        (define-key lsp-mode-map (kbd "C-c l") lsp-command-map)
        (setq lsp-file-watch-threshold 100000)
      '';
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
      '';
    };
  };
}
