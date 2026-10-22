{ config, lib, ... }:
{
  # Make `emacsclient -t` terminal frames first-class for remote work:
  # - kkp: Kitty Keyboard Protocol, so Ghostty delivers C-., C-;, S-RET, etc.
  # - OSC 52 under tmux, so kills reach the outer terminal's clipboard.
  config = lib.mkIf config.profiles.base.enable {
    programs.emacs.init = {
      # Under TERM=tmux-* Emacs skips the capability probe and uses this fixed
      # list, which lacks setSelection by default; tmux (set-clipboard on) and
      # mosh both pass OSC 52 through to Ghostty.
      prelude = ''
        (setopt xterm-tmux-extra-capabilities '(modifyOtherKeys setSelection))
      '';

      usePackage = {
        kkp = {
          enable = true;
          config = ''
            (global-kkp-mode +1)
          '';
        };
      };
    };
  };
}
