{
  config,
  lib,
  pkgs,
  ...
}:
{
  config = lib.mkIf config.profiles.base.enable {
    home.packages = with pkgs; [ signal-desktop ];

    home.shellAliases.open = "${pkgs.xdg-utils}/bin/xdg-open";

    programs.git.ignores = pkgs.gitignores "Global/Linux";

    services.emacs = {
      enable = true;
      client.enable = true;
    };

    services.gpg-agent = {
      enable = true;
      defaultCacheTtl = 4 * 60 * 60;
      maxCacheTtl = 4 * 60 * 60;
    };
  };
}
