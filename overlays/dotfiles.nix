self: super: {
  mcwitt-dotfiles = (super.fetchFromGitHub {
    owner = "mcwitt";
    repo = "dotfiles";
    rev = "8a88f793ebf8ab30d6dfe89af5e14f45d3cff8a5";
    sha256 = "1w57lklkb4s0yncfbyx56dvwq2gg92nb019ayz4khxr0ry995xnn";
  });
}
