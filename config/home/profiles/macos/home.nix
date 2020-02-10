{ pkgs, lib, ... }: {
  imports = [ ../../home.nix ];

  programs.git.ignores = pkgs.ghGitIgnoreLines "Global/macOS";
}
