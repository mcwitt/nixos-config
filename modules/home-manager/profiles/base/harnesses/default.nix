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
    ./opencode
  ];

  # Skill bundles shared across all harnesses, exposed as module args so each
  # harness composes the same set (see ./claude-code, ./codex, ./opencode).
  _module.args.gwsSkills = lib.mapAttrs' (
    name: _: lib.nameValuePair name "${pkgs.gws.src}/skills/${name}"
  ) (lib.filterAttrs (name: _: gwsInclude name) (builtins.readDir "${pkgs.gws.src}/skills"));

  _module.args.superpowersSkills = lib.mapAttrs' (
    name: _: lib.nameValuePair name "${inputs.superpowers}/skills/${name}"
  ) (builtins.readDir "${inputs.superpowers}/skills");

  _module.args.localSkills = {
    nixify = ./skills/nixify;
  };

  home.file = lib.mkIf cfg.enable (
    let
      memexSkill.source = config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/projects/memex";
    in
    {
      ".claude/skills/memex" = memexSkill;
      ".codex/skills/memex" = memexSkill;
      ".config/opencode/skills/memex" = memexSkill;
    }
  );

  home.packages = lib.mkIf cfg.enable [
    pkgs.gws
  ];

  programs.mcp = lib.mkIf cfg.enable {
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
}
