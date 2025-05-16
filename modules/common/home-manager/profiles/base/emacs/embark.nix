{ config, lib, ... }:
{
  config = lib.mkIf config.profiles.base.enable {
    programs.emacs.init.usePackage = {
      embark = {
        enable = true;
        bind = {
          # modify standard bindings to avoid colliding with evil-collection
          "C-;" = "embark-act";
          "C-'" = "embark-dwim";
          "C-h B" = "embark-bindings";
        };
        init = ''
          ;; Optionally replace the key help with a completing-read interface
          (setq prefix-help-command #'embark-prefix-help-command)
        '';
        config = ''
          ;; Hide the mode line of the Embark live/completions buffers
          (add-to-list 'display-buffer-alist
                       '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                         nil
                         (window-parameters (mode-line-format . none))))
        '';
      };

      embark-consult = {
        enable = true;
        hook = [ "(embark-collect-mode . consult-preview-at-point-mode)" ];
      };

      frames-only-mode = {
        config = lib.mkBefore ''
          ;; Prevent error when attempting to open in a new frame
          ;; Must run before enabling mode
          (add-to-list 'frames-only-mode-use-window-functions #'embark-act)
        '';
      };
    };
  };
}
