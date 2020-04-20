self: super: {
  mcwitt-dotfiles = (super.fetchFromGitHub {
    owner = "mcwitt";
    repo = "dotfiles";
    rev = "23bc0a1490c5b11d1fed019dda00c011dc8379a0";
    sha256 = "1p9vwh8jqy8l39avipm1pcz5xcsc81q8dbmfgkqvpw11isq89ixa";
  });
}
