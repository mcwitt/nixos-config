{
  programs.emacs.init.usePackage = {

    format-all = {
      enable = true;
      diminish = [ "format-all-mode" ];
      hook = [
        "(prog-mode . format-all-mode)"
        "(format-all-mode . format-all-ensure-formatter)"
      ];
      extraConfig = ''
        :custom
        (format-all-show-errors 'never)
      '';
    };
  };
}
