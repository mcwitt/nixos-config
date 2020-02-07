self: super: {
  mcwitt-dotfiles = (super.fetchFromGitHub {
    owner = "mcwitt";
    repo = "dotfiles";
    rev = "5af1b67fce97ef0286628a28f05e193d7d6a5571";
    sha256 = "0kdl9s3w48ch0iqhl5lk8i81lkb7r02gvrwg352dfkp8q047xqpg";
  });
}
