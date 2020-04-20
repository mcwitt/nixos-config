self: super: {
  mcwitt-dotfiles = (super.fetchFromGitHub {
    owner = "mcwitt";
    repo = "dotfiles";
    rev = "c89377ff79a4eb9e83eb7b8952d83ec64c9f4e66";
    sha256 = "1g59mr19fsms62pjv5m9yf6khzajzbj01rn4x1bxmy76813033gx";
  });
}
