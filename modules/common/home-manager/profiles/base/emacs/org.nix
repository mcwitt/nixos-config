{ config, lib, ... }:
let
  orgDir = "${config.home.homeDirectory}/src/notes";
  orgRoamDir = "${orgDir}/org-roam";
in
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
          # [[info:org#Activation][org#Activation]]
          "C-c l" = "org-store-link";
          "C-c a" = "org-agenda";
          "C-c c" = "org-capture";
        };
        config = ''
          (setq org-directory "${orgDir}")
          (setq org-agenda-files '("${orgDir}/gtd.org"))

          ;; Add +PROJECT to default stuck projects definition
          (setq org-stuck-projects '("+LEVEL=2+PROJECT/-DONE" ("TODO" "NEXT" "NEXTACTION") nil ""))
        '';
      };

      org-roam = {
        enable = true;
        init = ''
          (make-directory "${orgRoamDir}" t)
          (setq org-roam-directory "${orgRoamDir}")

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
