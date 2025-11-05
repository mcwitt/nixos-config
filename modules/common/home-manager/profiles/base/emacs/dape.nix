{ config, lib, ... }:
{
  config = lib.mkIf config.profiles.base.enable {
    programs.emacs.init.usePackage = {
      dape.enable = true;
      emacs.config = ''
        (setopt window-sides-vertical t)
      '';
    };
  };
}
