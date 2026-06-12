{
  config,
  inputs,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.profiles.base;
  gwsServices = [
    "shared"
    "calendar"
    "docs"
    "gmail"
  ];
  gwsInclude = name: lib.any (s: name == "gws-${s}" || lib.hasPrefix "gws-${s}-" name) gwsServices;
in
{
  imports = [
    ./claude-code
    ./codex
    ./memex.nix
    ./opencode
  ];

  config = lib.mkMerge [
    {
      # Skill bundles shared across all harnesses, exposed as module args so
      # each harness composes the same set (see ./claude-code, ./codex,
      # ./opencode). Unconditional: the imported modules take these as function
      # arguments, so they must exist even when the profile is disabled.
      _module.args.gwsSkills = lib.mapAttrs' (
        name: _: lib.nameValuePair name "${pkgs.gws.src}/skills/${name}"
      ) (lib.filterAttrs (name: _: gwsInclude name) (builtins.readDir "${pkgs.gws.src}/skills"));

      _module.args.superpowersSkills = lib.mapAttrs' (
        name: _: lib.nameValuePair name "${inputs.superpowers}/skills/${name}"
      ) (builtins.readDir "${inputs.superpowers}/skills");

      _module.args.localSkills = {
        nixify = ./skills/nixify;
      };
    }

    (lib.mkIf cfg.enable {
      home.packages = [
        pkgs.gws
      ];

      programs.mcp = {
        enable = true;
        servers.playwright = {
          command = lib.getExe pkgs.playwright-mcp;
          args = [
            "--browser=chrome"
            "--executable-path=${lib.getExe pkgs.chromium}"
            "--headless"
          ];
        };
      };
    })
  ];
}
