{ pkgs, ... }:
{
  home.packages = [ pkgs.git-annex ];

  home.shellAliases.ga = "${pkgs.git-annex}/bin/git-annex";

  programs.git.attributes = [
    "* annex.largefiles=largerthan=100kb"
  ];
}
