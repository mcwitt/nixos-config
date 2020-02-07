self: super: {
  mcwitt-dotfiles = (super.fetchFromGitHub {
    owner = "mcwitt";
    repo = "dotfiles";
    rev = "ef07eefdea933250e374600c937bb32f52e1c08b";
    sha256 = "1y6y34rcrpy3z0vq1dfryh7hlazwa4za480icq06z9pkizkkyi2g";
  });
}
