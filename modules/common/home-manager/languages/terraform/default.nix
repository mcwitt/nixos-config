{ config, lib, pkgs, ... }:
with lib;
let cfg = config.languages.terraform;
in
{
  options.languages.terraform.enable = mkEnableOption "Terraform language environment";

  config = mkIf cfg.enable {

    programs.emacs.init.usePackage = {

      lsp-terraform = {
        enable = true;
        config = ''
          (setq lsp-terraform-server "${pkgs.terraform-ls}")
        '';
      };

      terraform-mode = {
        enable = true;
        mode = [ ''"\\.tf\\'"'' ];
      };
    };
  };
}
