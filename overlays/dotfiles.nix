self: super: {
  mcwitt-dotfiles = (super.fetchFromGitHub {
    owner = "mcwitt";
    repo = "dotfiles";
    rev = "e67320f72aadcf596622c2e751362f7e7b0fde7a";
    sha256 = "195y5q26qn577zi6zpf27vwsazgv8pjxrz1npi52zwyizgpi3j7d";
  });
}
