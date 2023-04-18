{ inputs }:
final: _: {
  gitignores = path:
    final.splitString "\n"
      (builtins.readFile "${inputs.gitignore}/${path}.gitignore");
}
