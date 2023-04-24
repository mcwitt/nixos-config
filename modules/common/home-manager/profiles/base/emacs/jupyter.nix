{
  programs.emacs.init.usePackage = {

    inheritenv.enable = true;

    jupyter = {
      enable = true;
      after = [ "inheritenv" ];
      config = ''
        ;; show results in separate frame
        (customize-set-variable 'jupyter-pop-up-frame t)

        ;; https://github.com/purcell/envrc/issues/12#issuecomment-770523826
        (inheritenv-add-advice 'jupyter-command)
      '';
    };
  };
}
