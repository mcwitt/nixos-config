self: super: {
  mcwitt-dotfiles = (super.fetchFromGitHub {
    owner = "mcwitt";
    repo = "dotfiles";
    rev = "bcf6ed8c20a1d8dcd3dd4dec96c9bbcec5498297";
    sha256 = "1b8y57fx86m1b8jc1ji7n0cjmpkvl6r8h0fhvk627raaccc3nb0l";
  });
}
