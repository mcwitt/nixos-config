self: super: {
  mcwitt-dotfiles = (super.fetchFromGitHub {
    owner = "mcwitt";
    repo = "dotfiles";
    rev = "439e543b842adf33de2dc66ece7c9616001fa7fa";
    sha256 = "0dhkvd25vnr8mgqkv642gw3plmdsbajcabcv6b0897s7z0ihnnbx";
  });
}
