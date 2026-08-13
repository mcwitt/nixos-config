{ config, lib, ... }:
{
  # Make `emacsclient -t` terminal frames first-class for remote work:
  # - kkp: Kitty Keyboard Protocol, so Ghostty delivers C-., C-;, S-RET, etc.
  config = lib.mkIf config.profiles.base.enable {
    programs.emacs.init.usePackage = {
      kkp = {
        enable = true;
        config = ''
          (global-kkp-mode +1)
        '';
      };
    };
  };
}
