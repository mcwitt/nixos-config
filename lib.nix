final: _: {
  gitignores = path:
    final.splitString "\n"
      (builtins.readFile "${final.github-gitignore}/${path}.gitignore");
}
