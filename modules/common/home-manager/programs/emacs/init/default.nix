{ nurNoPkgs, ... }:
{
  imports = [
    ./faces.nix
    nurNoPkgs.repos.rycee.hmModules.emacs-init
  ];
}
