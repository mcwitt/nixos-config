{ github-gitignore, lib }:
path:
lib.splitString "\n"
  (builtins.readFile "${github-gitignore}/${path}.gitignore")
