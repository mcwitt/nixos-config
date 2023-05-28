{
  imports = [
    # use company instead of corfu until sync issue with eglot is resolved
    # https://github.com/joaotavora/eglot/discussions/1127
    ./company.nix
    # ./corfu.nix

    ./consult.nix
    ./embark.nix
    ./vertico.nix
  ];

  programs.emacs.init.usePackage = {

    all-the-icons-completion = {
      enable = true;
      config = ''
        (all-the-icons-completion-mode)
      '';
    };

    cape = {
      enable = true;

      init = ''
        ;; Add `completion-at-point-functions', used by `completion-at-point'.
        ;; NOTE: The order matters!
        (add-to-list 'completion-at-point-functions #'cape-dabbrev)
        (add-to-list 'completion-at-point-functions #'cape-file)
        ;;(add-to-list 'completion-at-point-functions #'cape-elisp-block)
        ;;(add-to-list 'completion-at-point-functions #'cape-history)
        ;;(add-to-list 'completion-at-point-functions #'cape-keyword)
        ;;(add-to-list 'completion-at-point-functions #'cape-tex)
        ;;(add-to-list 'completion-at-point-functions #'cape-sgml)
        ;;(add-to-list 'completion-at-point-functions #'cape-rfc1345)
        ;;(add-to-list 'completion-at-point-functions #'cape-abbrev)
        ;;(add-to-list 'completion-at-point-functions #'cape-dict)
        ;;(add-to-list 'completion-at-point-functions #'cape-symbol)
        ;;(add-to-list 'completion-at-point-functions #'cape-line)
      '';

      bind = {
        "C-c p p" = "completion-at-point"; # capf
        "C-c p t" = "complete-tag"; # etags
        "C-c p d" = "cape-dabbrev"; # or dabbrev-completion
        "C-c p h" = "cape-history";
        "C-c p f" = "cape-file";
        "C-c p k" = "cape-keyword";
        "C-c p s" = "cape-symbol";
        "C-c p a" = "cape-abbrev";
        "C-c p l" = "cape-line";
        "C-c p w" = "cape-dict";
        "C-c p \\\\" = "cape-tex";
        "C-c p &" = "cape-sgml";
        "C-c p r" = "cape-rfc1345";
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

    orderless = {
      enable = true;
      init = ''
        (setq completion-styles '(orderless)
              completion-category-defaults nil
              completion-category-overrides '((file (styles . (partial-completion)))))
      '';
    };
  };
}
