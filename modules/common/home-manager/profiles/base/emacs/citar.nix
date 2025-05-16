{ config, lib, ... }:
{
  config = lib.mkIf config.profiles.base.enable {
    programs.emacs.init = {
      # HACK: workaround for native-comp of the package quickstart
      # file failing with citar
      #
      #     Type citar-indicator missing from # typeof-types!
      #
      # We disable packageQuickstart and add (package-activate-all) to
      # init.el. The latter is needed because the emacs-init module
      # sets package-enable-at-startup to nil when packageQuickstart
      # is false.
      packageQuickstart = lib.mkForce false;

      prelude = ''
        (package-activate-all)
      '';

      usePackage = {
        citar = {
          enable = true;
          hook = [
            ''(LaTeX-mode . citar-capf-setup)''
            ''(org-mode . citar-capf-setup)''
          ];
          bindLocal.org-mode-map."C-c b" = "#'org-cite-insert";
          custom = {
            org-cite-global-bibliography = '''("~/org/bib/references.bib")'';
            org-cite-insert-processor = "'citar";
            org-cite-follow-processor = "'citar";
            org-cite-activate-processor = "'citar";
            citar-bibliography = "org-cite-global-bibliography";
          };
        };

        citar-embark = {
          enable = true;
          after = [
            "citar"
            "embark"
          ];
          config = ''
            (citar-embark-mode)
          '';
          diminish = [ "citar-embark-mode" ];
        };

        citar-org-roam = {
          enable = true;
          after = [
            "citar"
            "org-roam"
          ];
          config = ''
            (citar-org-roam-mode)
          '';
          diminish = [ "citar-org-roam-mode" ];
        };
      };
    };
  };
}
