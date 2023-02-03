{
  programs.emacs.init.usePackage = {
    eglot.config = ''
      ;; workaround for eglot to recognize tree-sitter modes
      (defun eglot-alias-modes (src dst)
        "Add an alias for an existing mode that eglot recognizes"
        (add-to-list
         'eglot-server-programs
         (cons dst (cdr (assoc src eglot-server-programs)))))

      (eglot-alias-modes 'python-mode 'python-ts-mode)
      (eglot-alias-modes '(c++-mode c-mode) '(c++-ts-mode c-ts-mode))
    '';

    format-all.config = ''
      (add-to-list 'language-id--definitions '("Python" python-mode python-ts-mode))
      (add-to-list 'language-id--definitions '("C++" c++-mode c++-ts-mode))
    '';

    ligature.config = ''
      (ligature-set-ligatures 'python-ts-mode '("->" "==" ">=" "<="))
    '';

    treesit-auto = {
      enable = true;
      config = ''
        (global-treesit-auto-mode)
      '';
    };
  };
}
