{
  config,
  lib,
  pkgs,
  ...
}:
{
  config = lib.mkIf config.profiles.base.enable {
    programs.emacs.overrides = final: prev: {

      base16-theme = final.trivialBuild rec {
        pname = "base16-theme";
        version = "3.1";

        src = pkgs.fetchFromGitHub {
          owner = "tinted-theming";
          repo = "base16-emacs";
          rev = "refs/tags/${version}";
          hash = "sha256-QLe9ywRv6Cne4Z0xPiCFp6owRFJQCHiCrfPl5bdTCfs=";
        };

        patches = [ ./base16-emacs/0001-Replace-nil-with-unspecified.patch ];
      };

      # https://github.com/danth/stylix/blob/c80d054fd4b62a4e8e1accf9c22b2e622737aadd/modules/emacs/hm.nix#L14-L55
      base16-stylix-theme = final.trivialBuild {
        pname = "base16-stylix-theme";
        version = "0.1";
        src =
          with config.lib.stylix.colors.withHashtag;
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

      prelude =
        let
          inherit (config.stylix) fonts;
        in
        ''
          (set-face-attribute 'default
                              nil
                              :family "${fonts.monospace.name}")
                              :height ${toString (fonts.sizes.applications * 10)}
          (set-face-attribute 'variable-pitch
                              nil
                              :family "${fonts.sansSerif.name}")
        '';

      usePackage.base16-stylix-theme = {
        enable = true;
        config = ''
          (load-theme 'base16-stylix t)
        '';
      };
    };

    stylix.targets.emacs.enable = false; # use our own configuration
  };
}
