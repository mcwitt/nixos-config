{ inputs, lib }:
{
  gitignores = path:
    lib.splitString "\n"
      (builtins.readFile "${inputs.gitignore}/${path}.gitignore");

  setAll = value: keys: builtins.listToAttrs
    (map
      (key: lib.nameValuePair key value)
      keys);
}
