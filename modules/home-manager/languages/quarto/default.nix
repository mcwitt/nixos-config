{
  config,
  lib,
  ...
}:
with lib;
let
  cfg = config.languages.quarto;
in
{
  options.languages.quarto.enable = mkEnableOption "Quarto language environment";

  config = mkIf cfg.enable {

    programs.emacs.init.usePackage.quarto-mode = {
      enable = true;
      mode = [ ''("\\.qmd\\'" . poly-quarto-mode)'' ];
      command = [ "quarto-preview" ];
    };
  };
}
