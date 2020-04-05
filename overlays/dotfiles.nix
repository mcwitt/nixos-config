self: super: {
  mcwitt-dotfiles = (super.fetchFromGitHub {
    owner = "mcwitt";
    repo = "dotfiles";
    rev = "b4d99d68a5a49c577cc50028acc681b71e517de0";
    sha256 = "18nab8in4y12kcbsaq8a2ck3q6g3g8w9xvgf2jfj2brx2fqqjqp9";
  });
}
