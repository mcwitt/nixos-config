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
      nix-linter
      nix-prefetch-git
      nix-prefetch-github
      nixfmt
      nixpkgs-fmt
    ];

    programs.emacs.init.usePackage = {

      nix-mode = {
        enable = true;
        mode = [ ''"\\.nix\\'"'' ];
        bindLocal.nix-mode-map = {
          "C-c C-z" = "nix-repl-show";
        };
      };

      nix-repl = {
        enable = true;
        command = [ "nix-repl" "nix-repl-show" ];
      };

      subword.hook = [ "(nix-mode . subword-mode)" ];
    };
  };
}
