{ config, pkgs, ... }:
{
  programs.git.ignores = pkgs.gitignores "Global/macOS";
}
