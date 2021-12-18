let sources = import ../../nix/sources.nix;
in
_: pkgs: {
  nur = import sources.nur { inherit pkgs; };
}
