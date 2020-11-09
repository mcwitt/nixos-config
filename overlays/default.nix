let path = ./overlays.d;
in
with builtins;
map
  (n: import (path + ("/" + n)))
  (filter
    (n: match ".*\\.nix" n != null || pathExists (path + ("/" + n + "/default.nix")))
    (attrNames (readDir path)))
