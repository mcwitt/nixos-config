self: super: {
  mcwitt-dotfiles = (super.fetchFromGitHub {
    owner = "mcwitt";
    repo = "dotfiles";
    rev = "d41bc4662fb21fc5476c5c55d5aacb5ba543a104";
    sha256 = "1008al58iiyxla8n9vylvybz1r77brviqd16kgbzaspzj7mkpv5v";
  });
}
