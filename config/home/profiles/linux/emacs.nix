{ pkgs, ... }: {
  imports = [ ../../emacs.nix ];
  services.emacs.enable = true;
}
