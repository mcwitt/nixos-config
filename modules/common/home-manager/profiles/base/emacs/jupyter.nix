{
  programs.emacs.init.usePackage = {
    jupyter = {
      enable = true;
      command = [ "jupyter-run-repl" ];
      bind = { "C-c j" = "jupyter-run-repl"; };
    };

    jupyter-tramp = {
      enable = true;
      command = [ "jupyter-tramp-file-name-p" ];
    };


    ob-async.config = ''
      (setq ob-async-no-async-languages-alist '("jupyter-python"))
    '';

    ob-jupyter = {
      enable = true;
      config = ''
        (setq org-babel-default-header-args:jupyter-python '((:async . "yes")
                                                             (:kernel . "python")
                                                             (:session . "default")))
      '';
    };
  };
}
