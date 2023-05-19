{ config, lib, ... }:
{
  options.programs.emacs.init.completion.company.enable = lib.mkEnableOption "Use company for completion";

  config = lib.mkIf config.programs.emacs.init.completion.company.enable {
    programs.emacs.init.usePackage.company = {
      enable = true;
      diminish = [ "company-mode" ];
      config = ''
        (global-company-mode)
      '';
    };
  };
}
