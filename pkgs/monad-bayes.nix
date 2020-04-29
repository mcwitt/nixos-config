{ callCabal2nix, fetchFromGitHub, ... }:
let
  src = fetchFromGitHub {
    owner = "tweag";
    repo = "monad-bayes";
    rev = "1c195314257b7f1ebc059079da85ac18f8fd4109";
    sha256 = "0lxym5yzzm916ck5mxsdalrxmwk6gqc24jb71zmz44m77a200alm";
  };
in callCabal2nix "monad-bayes" src { }
