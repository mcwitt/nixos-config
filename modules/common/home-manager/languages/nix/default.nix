{ config, lib, pkgs, ... }:
with lib;
let cfg = config.languages.nix;
in
{
  options.languages.nix.enable = mkEnableOption "Nix language environment";
  config = mkIf cfg.enable {
    home.packages = with pkgs; [
      cachix
      niv
      nix-info
      nix-prefetch-git
      nix-prefetch-github
      nixfmt
      nixpkgs-fmt
    ];

    programs.emacs.init.usePackage = {
      nix-mode = {
        enable = true;
        mode = [ ''"\\.nix\\'"'' ];
        hook = [
          "(nix-mode . nixpkgs-fmt-on-save-mode)"
          "(nix-mode . subword-mode)"
        ];
      };

      reformatter = {
        enable = true;
        config = ''
          (reformatter-define nixpkgs-fmt
            :program "nixpkgs-fmt"
            :lighter " NixpkgsFmt")
        '';
      };
    };
  };
}
