{ lib, ... }:
{
  programs.emacs.init.usePackage = {

    format-all = {
      enable = true;
      diminish = [ "format-all-mode" ];
      hook = [
        "(prog-mode . format-all-mode)"
        "(format-all-mode . format-all-ensure-formatter)"
      ];
    };

    frames-only-mode.config = lib.mkBefore ''
      (add-to-list 'frames-only-mode-use-window-functions 'format-all--buffer-from-hook)
    '';
  };
}
