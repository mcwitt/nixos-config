self: super: {
  mcwitt-dotfiles = (super.fetchFromGitHub {
    owner = "mcwitt";
    repo = "dotfiles";
    rev = "396de1ea106116b2eb3e82e4481e8717c4f3f89c";
    sha256 = "15in645q7y5qacnccbm756pbscpsh1hskwn34ynddphvjnry8ykl";
  });
}
