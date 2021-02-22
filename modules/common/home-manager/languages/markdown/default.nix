{ config, lib, pkgs, ... }:
with lib;
let cfg = config.languages.markdown;
in
{
  options.languages.markdown.enable =
    mkEnableOption "Markdown language environment";

  config = mkIf cfg.enable {
    home.packages = [ pkgs.mdl ];

    programs.emacs.init.usePackage = {
      markdown-mode = {
        enable = true;
        command = [ "markdown-mode" "gfm-mode" ];
        hook = [
          "(markdown-mode . turn-on-visual-line-mode)"
          "(markdown-mode . turn-on-flyspell)"
        ];
        mode = [
          ''("README\\.md\\'" . gfm-mode)''
          ''("\\.md\\'" . markdown-mode)''
          ''("\\.markdown\\'" . markdown-mode)''
        ];
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
