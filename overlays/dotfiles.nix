self: super: {
  mcwitt-dotfiles = (super.fetchFromGitHub {
    owner = "mcwitt";
    repo = "dotfiles";
    rev = "ababb8fed1579623aa22d51a9f2aecb1eda2aa5d";
    sha256 = "1vsskrjy0wq51qhsh1qmdyqkvar6pfgq3zmgpm29i6f09jqyhsrn";
  });
}
