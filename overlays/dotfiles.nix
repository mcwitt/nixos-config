self: super: {
  mcwitt-dotfiles = (super.fetchFromGitHub {
    owner = "mcwitt";
    repo = "dotfiles";
    rev = "e408f9e4c8048ad61acef8ca207a58ac0d4a2202";
    sha256 = "1vhvjsxf2a7gmgqxbl16ak9z3j3a402ng8w5pk5k60xyvvm54207";
  });
}
