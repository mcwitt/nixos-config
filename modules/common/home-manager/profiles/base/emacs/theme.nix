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
                              :family "${fonts.serif.name}")
        '';

      modus-themes = {
        enable = true;
        demand = true;
        bind."<f5>" = "modus-themes-toggle";
        custom.modus-themes-to-toggle = '''(modus-operandi-tinted modus-vivendi-tinted)'';
        config = ''
          (load-theme 'modus-operandi-tinted :no-confirm)
        '';
      };
    };

    stylix.targets.emacs.enable = false; # use our own configuration
  };
}
