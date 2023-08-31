{
  # user for home-assistant to log in as
  users.users.hass = {
    isNormalUser = true;
    extraGroups = [ "wheel" ];
  };
}
