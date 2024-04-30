{
  programs.emacs.init.usePackage.company = {
    enable = true;
    diminish = [ "company-mode" ];
    config = ''
      (global-company-mode)
    '';
  };
}
