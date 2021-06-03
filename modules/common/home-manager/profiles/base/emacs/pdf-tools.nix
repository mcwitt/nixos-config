{
  programs.emacs.init.usePackage = {

    pdf-annot = {
      enable = true;
      command = [ "pdf-annot-minor-mode" ];
    };

    pdf-history = {
      enable = true;
      command = [ "pdf-history-minor-mode" ];
    };

    pdf-links = {
      enable = true;
      command = [ "pdf-links-minor-mode" ];
    };

    pdf-occur = {
      enable = true;
      command = [ "pdf-occur-global-minor-mode" ];
    };

    pdf-outline = {
      enable = true;
      command = [ "pdf-outline-minor-mode" ];
    };

    pdf-sync = {
      enable = true;
      command = [ "pdf-sync-minor-mode" ];
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
  };
}
