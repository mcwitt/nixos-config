{ config, lib, ... }:
{
  config = lib.mkIf config.profiles.base.enable {

    programs.emacs.init.usePackage = {

      frames-only-mode.config = lib.mkBefore ''
        (add-to-list 'frames-only-mode-use-window-functions #'org-capture)
      '';

      org = {
        enable = true;
        bind = {
          # Standard global bindings
          # https://orgmode.org/manual/Activation.html#Activation-1
          "C-c l" = "org-store-link";
          "C-c a" = "org-agenda";
          "C-c c" = "org-capture";
        };
      };

      org-roam = {
        enable = true;
        init = ''
          (let ((d "~/src/notes/org-roam"))
            (progn
              (make-directory d t)
              (setq org-roam-directory (file-truename d))))

          (setq org-roam-dailies-directory "daily/")

          (setq org-roam-dailies-capture-templates
                '(("d" "default" entry
                   "* %?"
                   :target (file+head "%<%Y-%m-%d>.org"
                                      "#+title: %<%Y-%m-%d>\n"))))
        '';
        config = ''
          (org-roam-db-autosync-mode)
        '';
      };
    };
  };
}
