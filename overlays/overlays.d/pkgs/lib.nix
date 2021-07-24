{ lib }:
with lib;
{
  setAll = value: keys: listToAttrs
    (map
      (key: nameValuePair key value)
      keys);
}
