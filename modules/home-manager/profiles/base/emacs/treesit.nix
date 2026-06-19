{ config, lib, ... }:
{
  config = lib.mkIf config.profiles.base.enable {

    programs.emacs = {

      extraPackages = epkgs: [ epkgs.treesit-grammars.with-all-grammars ];

      init.usePackage = {
        treesit = {
          enable = true;
          config = ''
            (setopt treesit-font-lock-level 3)

            ;; Grammars are installed declaratively (with-all-grammars); don't
            ;; set treesit-auto-install-grammar.
            (setopt treesit-enabled-modes t)
          '';
        };
      };
    };
  };
}
