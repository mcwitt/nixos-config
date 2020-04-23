self: super: {
  mcwitt-dotfiles = (super.fetchFromGitHub {
    owner = "mcwitt";
    repo = "dotfiles";
    rev = "c57c4dda077a0b40957adfe831a8b1bb83e879a8";
    sha256 = "032j61xg0p4lqfaw696a6pyppphblqm9al1zmqg6qkp9i68gwyrc";
  });
}
