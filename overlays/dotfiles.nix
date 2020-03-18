self: super: {
  mcwitt-dotfiles = (super.fetchFromGitHub {
    owner = "mcwitt";
    repo = "dotfiles";
    rev = "8db18f527466d26b3ec256ee844f3b9b05ef9014";
    sha256 = "0m8vhq7h67c1yw6kcr7q3gb3hjrbpamkwpjvzqr6w4z9127wsknp";
  });
}
