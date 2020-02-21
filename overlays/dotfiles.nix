self: super: {
  mcwitt-dotfiles = (super.fetchFromGitHub {
    owner = "mcwitt";
    repo = "dotfiles";
    rev = "770aa073ce9218084a27fd0d4fb80e133d647a7d";
    sha256 = "0pfjkz2fixn25icl41w96l1m81f8v5l66nbp206gigyz29sdww92";
  });
}
