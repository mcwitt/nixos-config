self: super: {
  mcwitt-dotfiles = (super.fetchFromGitHub {
    owner = "mcwitt";
    repo = "dotfiles";
    rev = "c76c4fd984c709764a3e683a6bd74d13e03d7972";
    sha256 = "0ayi9zy6aivv9264ncdbvhnfc7pl7hlv5gdxamg146r3paq2abx8";
  });
}
