{
  config,
  gwsSkills,
  lib,
  ...
}:
let
  cfg = config.profiles.base;
in
{
  config = lib.mkIf cfg.enable {
    programs.opencode = {
      enable = true;
      enableMcpIntegration = true;

      skills = gwsSkills;
    };
  };
}
