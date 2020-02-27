self: super: {
  mcwitt-dotfiles = (super.fetchFromGitHub {
    owner = "mcwitt";
    repo = "dotfiles";
    rev = "ed5e88c7caf9c9d85022fd5477e5b5c423b2a049";
    sha256 = "0ias66ns1zh9kcm0vgxsfjhbpgzjvzjpsa9lwvb9jcjqmp16p4xp";
  });
}
