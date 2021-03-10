self: super:
let sources = import ../../nix/sources.nix;
in { nur = import sources.nur { pkgs = super; }; }
