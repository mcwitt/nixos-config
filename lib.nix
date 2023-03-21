{ inputs }:
final: _: {
  gitignores = path:
    final.splitString "\n"
      (builtins.readFile "${inputs.gitignore}/${path}.gitignore");

  setAll = value: keys: builtins.listToAttrs
    (map
      (key: final.nameValuePair key value)
      keys);
}
