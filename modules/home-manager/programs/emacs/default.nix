let
  nurNoPkgs =
    import (import ../../../../overlays/overlays.d/nix/sources.nix).nur { };
in
{ imports = [ ./org-protocol nurNoPkgs.repos.rycee.hmModules.emacs-init ]; }
