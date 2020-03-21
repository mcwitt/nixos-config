self: super: {
  mcwitt-dotfiles = (super.fetchFromGitHub {
    owner = "mcwitt";
    repo = "dotfiles";
    rev = "2f9547bc2e154604049fe4f2291f109cf3f8ba94";
    sha256 = "14vdqhd4cv94php8x2pzr15cw05qlyj5kz06shpfvikchgkkv8lh";
  });
}
