{
  # allow access to serial device (for Arduino dev)
  users.users.matt.extraGroups = [ "dialout" ];
  users.groups.dialout = { };
}
