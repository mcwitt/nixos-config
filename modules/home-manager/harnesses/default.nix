{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.harnesses;
  gwsServices = [
    "shared"
    "calendar"
    "docs"
    "gmail"
  ];
  gwsInclude = name: lib.any (s: name == "gws-${s}" || lib.hasPrefix "gws-${s}-" name) gwsServices;
  gwsSkills = lib.mapAttrs' (name: _: lib.nameValuePair name "${pkgs.gws.src}/skills/${name}") (
    lib.filterAttrs (name: _: gwsInclude name) (builtins.readDir "${pkgs.gws.src}/skills")
  );
in
{
  imports = [
    ./claude-code
    ./codex
    ./memex.nix
    ./opencode
    ./pi
  ];

  options.harnesses = {
    enable = lib.mkEnableOption "the agent CLI harnesses (claude-code, codex, opencode, pi)";

    skills = lib.mkOption {
      type = lib.types.attrsOf lib.types.path;
      default = { };
      example = lib.literalExpression ''
        {
          my-skill = ./skills/my-skill;
        }
      '';
      description = ''
        Skill directories (each containing a `SKILL.md`) shared across every
        supported harness. Definitions merge, so downstream flakes can add
        their own skills alongside the bundles set here.
      '';
    };
  };

  config = lib.mkIf cfg.enable {
    harnesses.skills = gwsSkills // {
      nixify = ./skills/nixify;
    };

    home.packages = [
      pkgs.gws
    ];

    programs.mcp = {
      enable = true;
      servers.playwright = {
        command = lib.getExe pkgs.playwright-mcp;
        args = [
          "--browser=chrome"
          "--headless"
        ]
        # chromium is not available on darwin; let playwright resolve its own
        # browser there rather than pinning the nixpkgs build.
        ++ lib.optionals pkgs.stdenv.isLinux [
          "--executable-path=${lib.getExe pkgs.chromium}"
        ];
      };
    };
  };
}
