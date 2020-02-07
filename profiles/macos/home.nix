{ pkgs, lib, ... }: {
  imports = [ ../../config/home.nix ];

  programs.git.ignores = pkgs.ghGitIgnore "Global/macOS";
}
