self: super:
with super.lib; {
  ghGitIgnore = super.fetchFromGitHub {
    owner = "github";
    repo = "gitignore";
    rev = "7ab549fcae8269fdd4004065470176f829d88200";
    sha256 = "01cawgj4y1m60gj3sjlzzpdib5j9wn43y0gdvi6iy07niav5sc42";
  };

  ghGitIgnoreLines = path:
    splitString "\n" (builtins.readFile "${self.ghGitIgnore}/${path}.gitignore");
}
