{ pkgs, lib, ... }: {
  imports = [ ../../config/home.nix ];

  programs.git.ignores = pkgs.ghGitIgnoreLines "Global/macOS";
}
