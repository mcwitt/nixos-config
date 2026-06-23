{ config, lib, ... }:
{
  # Make `emacsclient -t` terminal frames first-class for remote work:
  # - kkp: Kitty Keyboard Protocol, so Ghostty delivers C-., C-;, S-RET, etc.
  # - clipetty: routes the kill-ring to the system clipboard over SSH via OSC-52.
  config = lib.mkIf config.profiles.base.enable {
    programs.emacs.init.usePackage = {
      kkp = {
        enable = true;
        config = ''
          (global-kkp-mode +1)
        '';
      };

      clipetty = {
        enable = true;
        diminish = [ "clipetty-mode" ];
        config = ''
          (global-clipetty-mode +1)
        '';
      };
    };
  };
}
