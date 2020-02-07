self: super: {
  mcwitt-dotfiles = (super.fetchFromGitHub {
    owner = "mcwitt";
    repo = "dotfiles";
    rev = "8e065aa896c2f931d5a18e0643b9f0d913e079b9";
    sha256 = "0fffspz1d4saafpqwwx0rp3mj2a6l879rg4c9pmvbq6jcsd4153b";
  });
}
