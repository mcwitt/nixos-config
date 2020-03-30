self: super: {
  mcwitt-dotfiles = (super.fetchFromGitHub {
    owner = "mcwitt";
    repo = "dotfiles";
    rev = "d9521191e49c6585fd131c4ab7637d0b1a1e1e71";
    sha256 = "1c64z4mps2cppd0wm6c34rpn9cqjf6sl48gy6lap50nh49d80mfq";
  });
}
