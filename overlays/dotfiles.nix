self: super: {
  mcwitt-dotfiles = (super.fetchFromGitHub {
    owner = "mcwitt";
    repo = "dotfiles";
    rev = "99d22f5178711bfeb913167a2f8fff31424ba803";
    sha256 = "06dxk3j1mhphdk9qh6wqfmjdlc7z589cyw46wdq7myj6a8wh4x9m";
  });
}
