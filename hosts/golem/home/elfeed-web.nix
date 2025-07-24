{
  programs.emacs.init.usePackage.elfeed-web = {
    enable = true;
    demand = true;
    config = ''
      (elfeed-web-start)
    '';
  };
}
