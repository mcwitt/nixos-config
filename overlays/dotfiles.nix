self: super: {
  mcwitt-dotfiles = (super.fetchFromGitHub {
    owner = "mcwitt";
    repo = "dotfiles";
    rev = "64c1d1187c5d625c19ee44d9f2b5d974462a206d";
    sha256 = "0bplji3dj61ly2mahrhs5p5ixqyw80k7xmfhzky4kladdngilh8i";
  });
}
