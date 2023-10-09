# TODO: migrate once https://github.com/NixOS/nixpkgs/pull/160346 is merged
{ config, lib, inputs, ... }:
{
  config = lib.mkIf config.profiles.home-automation.enable {
    systemd.services.home-assistant.preStart =
      let inherit (config.services.home-assistant) configDir;
      in ''
        mkdir -p ${configDir}/custom_components
        ln -fns ${inputs.home-assistant-eero}/custom_components/eero ${configDir}/custom_components/
      '';
  };
}
