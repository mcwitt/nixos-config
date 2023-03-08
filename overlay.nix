{ inputs }:
final: prev:
let inherit (final) system;
  pkgs-unstable = inputs.nixpkgs-unstable.legacyPackages.${system};
in
{
  inherit (pkgs-unstable) wezterm;

  lib = prev.lib.extend
    (final: _: {
      gitignores = path:
        final.splitString "\n"
          (builtins.readFile "${inputs.gitignore}/${path}.gitignore");

      setAll = value: keys: builtins.listToAttrs
        (map
          (key: final.nameValuePair key value)
          keys);
    });
}
