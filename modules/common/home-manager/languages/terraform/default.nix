{ config, lib, pkgs, ... }:
with lib;
let cfg = config.languages.terraform;
in
{
  options.languages.terraform.enable = mkEnableOption "Terraform language environment";
  config = mkIf cfg.enable {
    home.packages = with pkgs; [ terraform ];
    programs.emacs.init.usePackage = {
      terraform-mode = {
        enable = true;
        mode = [ ''"\\.tf\\'"'' ];
      };
    };
  };
}
