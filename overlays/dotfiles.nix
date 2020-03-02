self: super: {
  mcwitt-dotfiles = (super.fetchFromGitHub {
    owner = "mcwitt";
    repo = "dotfiles";
    rev = "f2a2e021dd036139f90dfa78660b05a7a5c74cce";
    sha256 = "0j20v97p0cy97za11rnfz8wl5fn4cik8wc8ns14w639kznj87jmv";
  });
}
