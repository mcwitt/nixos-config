{
  programs.emacs.init.usePackage = {

    format-all = {
      enable = true;
      diminish = [ "format-all-mode" ];
      custom.format-all-show-errors = "'never";
      hook = [
        "(prog-mode . format-all-mode)"
        "(format-all-mode . format-all-ensure-formatter)"
      ];
    };
  };
}
