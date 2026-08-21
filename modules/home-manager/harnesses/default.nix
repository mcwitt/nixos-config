{
  config,
  inputs,
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
  # GitHub renders soft line breaks as <br> in comment fields, though not in
  # .md files, so hard-wrapped prose arrives ragged where people actually read
  # it. One copy of the rule, shared by every harness that takes a context file.
  context = ''
    ## Markdown posted to GitHub

    GitHub renders soft line breaks as `<br>` in comment fields — issue and pull request bodies, review and comment text, release notes — though not in `.md` files committed to a repository. Write each paragraph as a single source line, with no intra-paragraph line breaks, in anything destined for GitHub. Code blocks, tables and list structure keep their own line breaks, and a list item is itself one line. Prefer `gh ... --body-file <file>` over an inline `--body`, so the text survives shell quoting intact.
  '';
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
    programs.claude-code.context = context;
    programs.codex.context = context;
    programs.opencode.context = context;

    harnesses.skills = gwsSkills // {
      asd-ste100 = "${inputs.asd-ste100}";
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
