self: super: {
  mcwitt-dotfiles = (super.fetchFromGitHub {
    owner = "mcwitt";
    repo = "dotfiles";
    rev = "2b42b909005414b73c5d232af64b38af085e8cfd";
    sha256 = "1s6q4106hai1qyjkr6vjhnz83iy7idp8zpc2x37i9hy6dmzmgpj9";
  });
}
