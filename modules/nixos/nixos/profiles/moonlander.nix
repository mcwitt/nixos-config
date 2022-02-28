{ config, lib, pkgs, ... }:
with lib;
let cfg = config.profiles.moonlander;
in
{
  options.profiles.moonlander = {
    enable = mkEnableOption "Config for Moonlander keyboard";
    allowedUsers = mkOption {
      default = [ ];
      type = with types; listOf str;
      description = "Users with access to train, flash, etc.";
    };
  };

  config = mkIf cfg.enable ({
    hardware.keyboard.zsa.enable = true;
    environment.systemPackages = [ pkgs.wally-cli ];
    users.users = mkMerge (map (u: { ${u}.extraGroups = [ "plugdev" ]; }) cfg.allowedUsers);
  });
}
