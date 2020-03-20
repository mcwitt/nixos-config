self: super: {
  mcwitt-dotfiles = (super.fetchFromGitHub {
    owner = "mcwitt";
    repo = "dotfiles";
    rev = "1036327d40ee7226366c351e71bbae4b66316303";
    sha256 = "1kl188w12p55vbz6dbvs8ip0hmya1bf1n9f32fq2pilcqqwf7rpy";
  });
}
