{
  config,
  lib,
  pkgs,
  ...
}:
{
  config = lib.mkIf config.profiles.desktop.enable {

    programs.firefox = {
      enable = true;
      configPath = "${config.xdg.configHome}/mozilla/firefox";
      profiles.default.extensions.packages = with pkgs.nur.repos.rycee.firefox-addons; [
        browserpass
        privacy-badger
        ublock-origin
        vimium
      ];
    };

    stylix.targets.firefox.profileNames = [ "default" ];
  };
}
