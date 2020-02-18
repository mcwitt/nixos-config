self: super: {
  mcwitt-dotfiles = (super.fetchFromGitHub {
    owner = "mcwitt";
    repo = "dotfiles";
    rev = "5d6feee8fff99b485bc87e354d035e7bd8cca167";
    sha256 = "15439ckwr3hal1wiyix3lbda5mnj2yv2cxglw4s3idycsng5ciix";
  });
}
