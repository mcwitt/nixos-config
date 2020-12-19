{
  programs.emacs.init.usePackage = {
    lsp-mode = {
      enable = true;
      command = [ "lsp" ];
      after = [ "company" "flycheck" ];
      hook = [
        "(lsp-mode . lsp-enable-which-key-integration)"
        "(lsp-mode . lsp-lens-mode)"
      ];
      init = ''
        (setq lsp-keymap-prefix "C-c l")
      '';
      config = ''
        (setq lsp-diagnostics-provider :flycheck)
        (setq lsp-file-watch-threshold 10000)
        (define-key lsp-mode-map (kbd "C-c l") lsp-command-map)
      '';
    };

    lsp-modeline = {
      enable = true;
      command = [ "lsp-modeline-workspace-status-mode " ];
    };

    lsp-ui = {
      enable = true;
      command = [ "lsp-ui-mode" ];
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
      hook = [ "(lsp-mode . dap-ui-mode)" ];
      command = [ "dap-ui-mode" ];
    };

    lsp-ivy = {
      enable = true;
      after = [ "lsp-mode" "ivy" ];
      command =
        [ "lsp-ivy-workspace-symbol" "lsp-ivy-global-workspace-symbol" ];
    };

    lsp-treemacs = {
      enable = true;
      after = [ "lsp-mode" "treemacs" ];
    };
  };
}
