{ config, lib, ... }:
{
  config = lib.mkIf config.profiles.base.enable {
    programs.emacs.init = {
      usePackage = {
        citar = {
          enable = true;
          hook = [
            "(LaTeX-mode . citar-capf-setup)"
            "(org-mode . citar-capf-setup)"
          ];
          bindLocal.org-mode-map."C-c b" = "#'org-cite-insert";
          config = ''
            (setopt org-cite-global-bibliography  '("~/org/bib/references.bib")
                    org-cite-insert-processor 'citar
                    org-cite-follow-processor 'citar
                    org-cite-activate-processor 'citar
                    citar-bibliography org-cite-global-bibliography)
          '';
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
