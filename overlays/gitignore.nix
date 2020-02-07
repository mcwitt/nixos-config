self: super:
with super.lib; {
  # TODO: I'm unsure if it makes sense to add this as an overlay. The
  # goal is to have 'ghGitIgnore' accessible from multiple
  # configuration files. This method seems convenient because it
  # bypasses the need to manually import this file into every module
  # where it's used (instead it's passed automatically via 'pkgs');
  # however, this is not really a package definition and might go
  # against some philosophy of Nix/nixpkgs
  ghGitIgnore = let
    repo = super.fetchFromGitHub {
      owner = "github";
      repo = "gitignore";
      rev = "7ab549fcae8269fdd4004065470176f829d88200";
      sha256 = "01cawgj4y1m60gj3sjlzzpdib5j9wn43y0gdvi6iy07niav5sc42";
    };
  in path: splitString "\n" (builtins.readFile "${repo}/${path}.gitignore");
}
