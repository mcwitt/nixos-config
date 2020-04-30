{ callCabal2nix, fetchFromGitHub, ... }:
let
  src = fetchFromGitHub {
    owner = "DougBurke";
    repo = "hvega";
    rev = "f132a5f87ee64c1f4095bf159fb5ca6221d0bad5";
    sha256 = "18pnhvv8lqm943mpbrjifi4hv0mq96ac0qk1yllx2igfdbdz4gyv";
  };
in {
  hvega = callCabal2nix "hvega" "${src}/hvega" { };
  ihaskell-hvega = callCabal2nix "ihaskell-hvega" "${src}/ihaskell-hvega" { };
}
