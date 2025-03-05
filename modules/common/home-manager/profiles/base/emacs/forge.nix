{ config, lib, ... }:
{
  config = lib.mkIf config.profiles.base.enable {
    programs.emacs.init.usePackage = {

      forge = {
        enable = true;
        after = [ "magit" ];
      };

      magit.init = ''
        (setq forge-add-default-bindings nil)
      '';
    };
  };
}
