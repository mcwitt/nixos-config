self: super: {
  mcwitt-dotfiles = (super.fetchFromGitHub {
    owner = "mcwitt";
    repo = "dotfiles";
    rev = "62d8903403b0efebc2410dd38d4bc3844c191547";
    sha256 = "0m2l44g4a6x90b0vc3lvqby62zm7qs9przp3ghjbkvm8mf0cv4ms";
  });
}
