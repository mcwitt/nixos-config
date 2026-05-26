{
  config,
  gwsSkills,
  inputs,
  lib,
  ...
}:
let
  cfg = config.profiles.base;
  superpowersSkills = lib.mapAttrs' (
    name: _: lib.nameValuePair name "${inputs.superpowers}/skills/${name}"
  ) (builtins.readDir "${inputs.superpowers}/skills");
in
{
  config = lib.mkIf cfg.enable {
    programs.codex = {
      enable = true;
      enableMcpIntegration = true;

      settings = {
        model = "gpt-5.5";
        model_reasoning_effort = "high";
        approval_policy = "on-request";
        sandbox_mode = "workspace-write";
      };

      skills = superpowersSkills // gwsSkills;
    };
  };
}
