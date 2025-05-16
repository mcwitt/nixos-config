{ config, lib, ... }:
{
  config = lib.mkIf config.profiles.base.enable {

    programs.emacs = {

      extraPackages = epkgs: [ epkgs.treesit-grammars.with-all-grammars ];

      init.usePackage = {
        treesit = {
          enable = true;
          config = ''
            (setq treesit-font-lock-level 3)
          '';
        };
      };
    };
  };
}
