{
  config,
  lib,
  ...
}:
{
  config = lib.mkIf config.profiles.base.enable {

    programs.emacs.init.usePackage = {

      emacs.config =
        let
          inherit (config.stylix) fonts;
        in
        ''
          (set-face-attribute 'default
                              nil
                              :family "${fonts.monospace.name}"
                              :height ${toString (fonts.sizes.applications * 10)})
          (set-face-attribute 'variable-pitch
                              nil
                              :family "${fonts.sansSerif.name}")
        '';

      modus-themes = {
        enable = true;
        config = ''
          (load-theme 'modus-operandi :no-confirm)
          (define-key global-map (kbd "<f5>") #'modus-themes-toggle)
        '';
      };
    };

    stylix.targets.emacs.enable = false; # use our own configuration
  };
}
