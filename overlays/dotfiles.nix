self: super: {
  mcwitt-dotfiles = (super.fetchFromGitHub {
    owner = "mcwitt";
    repo = "dotfiles";
    rev = "5f1e297372aa312ab95697557d8264016130cd6b";
    sha256 = "0canssdq8r07zim71km75h852ypxg8wyy9rh1cqccb47xv06airh";
  });
}
