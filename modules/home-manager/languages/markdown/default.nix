{
  config,
  lib,
  pkgs,
  ...
}:
with lib;
let
  cfg = config.languages.markdown;
in
{
  options.languages.markdown.enable = mkEnableOption "Markdown language environment";

  config = mkIf cfg.enable {

    home.packages = [ pkgs.mdl ];

    # markdown-appear: reveal hidden Markdown markup at point (org-appear-style).
    # Not in nixpkgs/MELPA; built from source. https://github.com/mcwitt/markdown-appear
    programs.emacs.overrides = self: _: {
      markdown-appear = self.trivialBuild {
        pname = "markdown-appear";
        version = "0.1.0";
        src = pkgs.fetchFromGitHub {
          owner = "mcwitt";
          repo = "markdown-appear";
          rev = "215bb3ab8e0db22e14777575f7dad8ef467a1709";
          hash = "sha256-w9VFiRzb8oIYU9cJwGb1VXF8Ilq3XUS7uXOs2dGuE9E=";
        };
        packageRequires = [ self.markdown-mode ];
      };
    };

    programs.emacs.init.usePackage = {
      markdown-mode = {
        enable = true;
        command = [
          "markdown-mode"
          "gfm-mode"
        ];
        hook = [
          "(markdown-mode . turn-on-visual-line-mode)"
          "(markdown-mode . flyspell-mode)"
        ];
        mode = [
          ''("README\\.md\\'" . gfm-mode)''
          ''("\\.md\\'" . markdown-mode)''
          ''("\\.markdown\\'" . markdown-mode)''
        ];
        config = ''
          ;; Hide markup (revealed at point by markdown-appear) and scale headings.
          (setopt markdown-hide-markup t
                  markdown-header-scaling t
                  ;; Smaller, standard-size list bullets; default glyphs are large.
                  markdown-list-item-bullets '("•" "◦" "▪"))
        '';
      };

      markdown-appear = {
        enable = true;
        hook = [ "(markdown-mode . markdown-appear-mode)" ];
      };

      org.config = ''
        (require 'ox-gfm)
      '';

      ox-gfm = {
        enable = true;
        after = [ "org" ];
      };
    };
  };
}
