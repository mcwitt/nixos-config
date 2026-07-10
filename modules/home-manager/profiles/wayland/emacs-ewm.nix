{
  config,
  lib,
  pkgs,
  ...
}:
{
  # Per-host output configuration, injected verbatim as the elisp value of
  # `ewm-output-config'. Hosts set this to a quoted alist, e.g.
  #   ''
  #     '(("DP-2" :width 3840 :height 2160 :scale 1.5 :x 0 :y 0)
  #       ("DP-0" :width 3840 :height 2160 :scale 1.5 :x 2560 :y 0))
  #   ''
  # or leave the default "nil" to let ewm auto-configure outputs.
  options.programs.ewmOutputConfig = lib.mkOption {
    type = lib.types.lines;
    default = "nil";
    description = "Elisp value for `ewm-output-config' (a quoted alist, or nil).";
  };

  config = lib.mkIf (config.profiles.wayland.enable && pkgs.stdenv.isLinux) {
    programs.emacs.init.usePackage.ewm = {
      enable = true;
      demand = true;
      config = ''
        (setopt ewm-output-config ${config.programs.ewmOutputConfig})
      '';
      extraConfig = ''
        :bind (:map ewm-mode-map
               ;; Launch / switch everything (Emacs-native idiom)
               ("s-d" . consult-buffer)
               ("s-<return>" . vterm)
               ;; Media keys (intercepted even when a Wayland surface is focused)
               ("<XF86MonBrightnessUp>" . my/ewm-brightness-up)
               ("<XF86MonBrightnessDown>" . my/ewm-brightness-down)
               ("<XF86AudioRaiseVolume>" . my/ewm-volume-up)
               ("<XF86AudioLowerVolume>" . my/ewm-volume-down)
               ("<XF86AudioMute>" . my/ewm-volume-mute)
               ;; Screenshots (flameshot replacement): Print = select region
               ;; and annotate; S-Print = full screen to clipboard.
               ("<print>" . my/ewm-screenshot-region)
               ("S-<print>" . my/ewm-screenshot-screen))
      '';
      init = ''
        (defun my/ewm-brightness-up ()
          (interactive)
          (start-process "brightness" nil "brightnessctl" "set" "5%+"))
        (defun my/ewm-brightness-down ()
          (interactive)
          (start-process "brightness" nil "brightnessctl" "set" "5%-"))
        (defun my/ewm-volume-up ()
          (interactive)
          (start-process "volume" nil "wpctl" "set-volume" "@DEFAULT_AUDIO_SINK@" "5%+"))
        (defun my/ewm-volume-down ()
          (interactive)
          (start-process "volume" nil "wpctl" "set-volume" "@DEFAULT_AUDIO_SINK@" "5%-"))
        (defun my/ewm-volume-mute ()
          (interactive)
          (start-process "volume" nil "wpctl" "set-mute" "@DEFAULT_AUDIO_SINK@" "toggle"))
        (defun my/ewm-screenshot-region ()
          "Select a region with slurp, capture it with grim, annotate in satty."
          (interactive)
          (start-process "screenshot" nil "sh" "-c"
                         "grim -g \"$(slurp)\" - | satty --filename -"))
        (defun my/ewm-screenshot-screen ()
          "Capture the full screen with grim and copy it to the clipboard."
          (interactive)
          (start-process "screenshot" nil "sh" "-c" "grim - | wl-copy"))
      '';
    };
  };
}
