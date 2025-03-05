{ config, lib, ... }:
{
  config = lib.mkIf config.profiles.base.enable {
    programs.git.attributes = [
      "*.ipynb -diff"
    ];
  };
}
