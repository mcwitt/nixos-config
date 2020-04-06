self: super: {
  mcwitt-dotfiles = (super.fetchFromGitHub {
    owner = "mcwitt";
    repo = "dotfiles";
    rev = "30b3dbdf9c08885b7a466e94b1f3d87b44ab96da";
    sha256 = "0wxh0rqizh06lnrsrp3gcwmkryqxghd7q82cp15ijv1adrmqqyfi";
  });
}
