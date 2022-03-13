let nurNoPkgs = import (import ../../../../../../nix/sources.nix).nur { }; in
{
  imports = [
    ./faces.nix
    nurNoPkgs.repos.rycee.hmModules.emacs-init
  ];
}
