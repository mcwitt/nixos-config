{ config, lib, ... }:
{
  config = lib.mkIf config.profiles.base.enable {
    programs.emacs.init.usePackage = {

      inheritenv.enable = true;

      jupyter = {
        enable = true;
        after = [ "inheritenv" ];
        custom.jupyter-pop-up-frame = "t"; # show results in separate frame
        config = ''
          ;; https://github.com/purcell/envrc/issues/12#issuecomment-770523826
          (inheritenv-add-advice 'jupyter-command)
        '';
      };
    };
  };
}
