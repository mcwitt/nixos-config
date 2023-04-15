{ config, pkgs, ... }:
{
  programs.emacs.overrides = final: prev: {

    # https://github.com/danth/stylix/blob/c80d054fd4b62a4e8e1accf9c22b2e622737aadd/modules/emacs/hm.nix#L14-L55
    base16-stylix-theme = final.trivialBuild {
      pname = "base16-stylix-theme";
      src = with config.lib.stylix.colors.withHashtag;
        pkgs.writeText "base16-stylix-theme.el" ''
          (require 'base16-theme)
          (defvar base16-stylix-theme-colors
            '(:base00 "${base00}"
              :base01 "${base01}"
              :base02 "${base02}"
              :base03 "${base03}"
              :base04 "${base04}"
              :base05 "${base05}"
              :base06 "${base06}"
              :base07 "${base07}"
              :base08 "${base08}"
              :base09 "${base09}"
              :base0A "${base0A}"
              :base0B "${base0B}"
              :base0C "${base0C}"
              :base0D "${base0D}"
              :base0E "${base0E}"
              :base0F "${base0F}")
            "All colors for Base16 stylix are defined here.")
          ;; Define the theme
          (deftheme base16-stylix)
          ;; Add all the faces to the theme
          (base16-theme-define 'base16-stylix base16-stylix-theme-colors)
          ;; Mark the theme as provided
          (provide-theme 'base16-stylix)
          ;; Add path to theme to theme-path
          (add-to-list 'custom-theme-load-path
              (file-name-directory
                  (file-truename load-file-name)))
          (provide 'base16-stylix-theme)
        '';
      packageRequires = [ final.base16-theme ];
    };
  };

  programs.emacs.init = {

    prelude = with config.stylix.fonts; ''
      (set-face-attribute 'default
                          nil
                          :family "${monospace.name}")
                          :height ${builtins.toString (sizes.applications * 10)}
      (set-face-attribute 'variable-pitch
                          nil
                          :family "${sansSerif.name}")
    '';

    usePackage.base16-stylix-theme = {
      enable = true;
      config = ''
        (load-theme 'base16-stylix t)
      '';
    };
  };

  stylix.targets.emacs.enable = false; # use our own configuration
}
