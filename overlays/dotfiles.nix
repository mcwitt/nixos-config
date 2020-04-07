self: super: {
  mcwitt-dotfiles = (super.fetchFromGitHub {
    owner = "mcwitt";
    repo = "dotfiles";
    rev = "1f26b326993c812fb38ae9ee8cf35f938a6298f8";
    sha256 = "01ljxv7nfw5y2hhz25j6kk25aca50bcwlaiafflgkjmwnj5ax1pm";
  });
}
