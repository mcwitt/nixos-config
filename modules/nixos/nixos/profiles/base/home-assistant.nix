{
  # user for home-assistant to log in as
  users = {
    users.hass = {
      isSystemUser = true;
      group = "hass";
      extraGroups = [ "wheel" ];
    };
    groups.hass = { };
  };
}
