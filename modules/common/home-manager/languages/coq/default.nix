{ config, lib, ... }:
with lib;
let
  cfg = config.languages.coq;
in
{
  options.languages.coq.enable = mkEnableOption "Coq language environment";

  config = mkIf cfg.enable {

    programs.emacs.init.usePackage = {

      proof-general = {
        enable = true;
        package = ps: ps.proof-general;
        init = ''
          (with-eval-after-load "proof-script"
            (bind-keys :map proof-mode-map
                       ("C-c C-c" . proof-goto-point)
                       ("C-c C-k" . proof-interrupt-process)))
        '';
      };

      evil.config = ''
        (setopt evil-want-abbrev-expand-on-insert-exit nil)
      '';
    };
  };
}
