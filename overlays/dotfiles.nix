self: super: {
  mcwitt-dotfiles = (super.fetchFromGitHub {
    owner = "mcwitt";
    repo = "dotfiles";
    rev = "958cf25314d57eec26f657cf857584932552d317";
    sha256 = "02bwpw9x6b9cdwxaq8iq8hkc4jxkvrjbfwfxp8yhf5kmma56fbjq";
  });
}
