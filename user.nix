{ lib, ... }:
with lib;
{
  options.user = {
    fullName = {
      first = mkOption {
        type = types.str;
        description = "First name";
      };
      last = mkOption {
        type = types.str;
        description = "Last name";
      };
    };
    email = mkOption
      {
        type = types.str;
        description = "Email";
      };
  };
}
