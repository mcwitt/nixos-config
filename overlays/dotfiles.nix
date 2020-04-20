self: super: {
  mcwitt-dotfiles = (super.fetchFromGitHub {
    owner = "mcwitt";
    repo = "dotfiles";
    rev = "84726ecf3aa506f2ea7e539817862695a14b4f83";
    sha256 = "19mpsid9286l1k29dq2wg240rfcb12rxrgzras23gm890ra6gf47";
  });
}
