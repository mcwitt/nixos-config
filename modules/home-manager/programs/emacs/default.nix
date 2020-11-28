let
  nurNoPkgs =
    import (import ../../../../nix/sources.nix).nur { };
in
{ imports = [ ./org-protocol nurNoPkgs.repos.rycee.hmModules.emacs-init ]; }
