{ config, lib, ... }:
with lib;
let
  cfg = config.languages.terraform;
in
{
  options.languages.terraform.enable = mkEnableOption "Terraform language environment";

  config = mkIf cfg.enable {

    programs.emacs.init.usePackage = {
      terraform-mode.enable = true;
    };
  };
}
