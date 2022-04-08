{ config, lib, ... }:
with lib;
{
  options.programs.emacs.init.faces.height = mkOption {
    type = types.ints.positive;
    default = 100;
  };

  config.programs.emacs.init.earlyInit = ''
    (set-face-attribute 'default
                        nil
                        :height ${builtins.toString config.programs.emacs.init.faces.height}
                        :family "Fira Code Nerd Font")
    (set-face-attribute 'variable-pitch
                        nil
                        :family "DejaVu Sans")
  '';
}