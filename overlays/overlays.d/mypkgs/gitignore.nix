{ lib, mypkgs }:
let sources = import ../../../nix/sources.nix;
in
{
  ghGitIgnoreLines = path:
    lib.splitString "\n"
      (builtins.readFile "${sources.github-gitignore}/${path}.gitignore");
}
