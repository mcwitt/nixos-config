{ config, lib, ... }:
{
  config = lib.mkIf config.profiles.base.enable {
    home.file.".emacs.d/snippets" = {
      source = ./snippets;
      recursive = true;
    };
  };
}
