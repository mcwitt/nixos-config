{ stdenv, fetchFromGitHub }:
stdenv.mkDerivation {
  name = "dotfiles";
  src = fetchFromGitHub {
    owner = "mcwitt";
    repo = "dotfiles";
    rev = "7784bf564464127322fd13b03d0a8f4fb260194d";
    sha256 = "0apx63gwhv66v6f0w2xhzh5cjlzhl3qz80ylc0wfcfczkccccvqk";
  };
  phases = [ "installPhase" ];
  installPhase = ''
    mkdir -p $out
    cp -r $src/* $out
  '';
}
