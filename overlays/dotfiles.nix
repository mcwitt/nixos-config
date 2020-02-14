self: super: {
  mcwitt-dotfiles = (super.fetchFromGitHub {
    owner = "mcwitt";
    repo = "dotfiles";
    rev = "a8b093abd0e9ede8d7da98e605a0df862143c8b4";
    sha256 = "0isnnn06ap8r0qvj82cr16a40aqq26iykhv47zrgjyla87a7vilv";
  });
}
