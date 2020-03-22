self: super: {
  mcwitt-dotfiles = (super.fetchFromGitHub {
    owner = "mcwitt";
    repo = "dotfiles";
    rev = "599059b0c59232555d8938a6e32b73ad85c0af66";
    sha256 = "106shi9jrx1b9938iqw5mc5l5manj1xphcy2r7rlrmdrgpicxsav";
  });
}
