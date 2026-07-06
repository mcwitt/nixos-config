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

    programs.emacs.overrides = _: super: {
      quarto-mode = super.quarto-mode.overrideAttrs (old: {
        postPatch = (old.postPatch or "") + ''
          substituteInPlace quarto-mode.el --replace-warn '"--no-watch-inputs"' ""
        '';
      });
    };

    programs.emacs.init.usePackage.quarto-mode = {
      enable = true;
      mode = [ ''("\\.qmd\\'" . poly-quarto-mode)'' ];
      command = [ "quarto-preview" ];
    };
  };
}
