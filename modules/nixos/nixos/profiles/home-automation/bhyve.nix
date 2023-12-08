{ config, inputs, lib, ... }:
{
  config = lib.mkIf config.profiles.home-automation.enable {
    systemd.services.home-assistant.preStart =
      let inherit (config.services.home-assistant) configDir;
      in ''
        mkdir -p ${configDir}/custom_components
        ln -fns ${inputs.bhyve-home-assistant}/custom_components/bhyve ${configDir}/custom_components/
      '';
  };
}
