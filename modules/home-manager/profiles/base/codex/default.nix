{
  config,
  inputs,
  lib,
  ...
}:
let
  cfg = config.profiles.base;
in
{
  config = lib.mkIf cfg.enable {
    programs.codex = {
      enable = true;

      settings = {
        model = "gpt-5.5";
        model_reasoning_effort = "high";
        approval_policy = "on-request";
        sandbox_mode = "workspace-write";
      };

      skills = "${inputs.superpowers}/skills";
    };
  };
}
