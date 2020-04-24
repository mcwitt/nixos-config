self: super: {
  mcwitt-dotfiles = (super.fetchFromGitHub {
    owner = "mcwitt";
    repo = "dotfiles";
    rev = "049c1cc70a2f3ba17e2f4219aa44d3c4d74f2292";
    sha256 = "17zhmvxp6lz31byadkp1hxca1cmsapdgsmz0bk2xljzsxhjpdwrr";
  });
}
