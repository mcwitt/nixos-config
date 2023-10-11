{ config, lib, ... }:
{
  config = lib.mkIf config.profiles.base.enable {
    services.pass-secret-service.enable = true;

    services.git-sync = {
      enable = true;
      repositories.password-store = {
        path = config.programs.password-store.settings.PASSWORD_STORE_DIR;
        interval = 300;
      };
    };
  };
}
