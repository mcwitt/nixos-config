{
  programs.emacs.init.usePackage = {

    eglot = {
      enable = true;
      command = [ "eglot" ];

      bindLocal.eglot-mode-map = {
        "C-c l a" = "eglot-code-actions";
        "C-c l r" = "eglot-rename";
      };

      config = ''
        ;; https://github.com/joaotavora/eglot/discussions/898
        (add-hook 'eglot-managed-mode-hook
                  (lambda ()
                    ;; Show flymake diagnostics first.
                    (setq eldoc-documentation-functions
                          (cons #'flymake-eldoc-function
                                (remove #'flymake-eldoc-function eldoc-documentation-functions)))
                    ;; Show all eldoc feedback.
                    (setq eldoc-documentation-strategy #'eldoc-documentation-compose)))
      '';
    };
  };
}
