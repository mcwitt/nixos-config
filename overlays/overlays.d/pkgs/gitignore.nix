{ lib, mypkgs }:
let inherit (mypkgs) sources;
in
{
  ghGitIgnoreLines = path:
    lib.splitString "\n"
      (builtins.readFile "${sources.github-gitignore}/${path}.gitignore");
}
