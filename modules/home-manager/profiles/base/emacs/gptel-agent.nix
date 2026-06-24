{
  config,
  gwsSkills,
  localSkills,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.profiles.base;
  # Curated skill set, mirroring the bundles the other harnesses share. The
  # symlink farm is a single parent dir; gptel-agent scans it recursively for
  # SKILL.md (following symlinks into the store).
  skillDir = pkgs.linkFarm "gptel-agent-skills" (gwsSkills // localSkills);
in
{
  config = lib.mkIf cfg.enable {
    programs.emacs.init.usePackage.gptel-agent = {
      enable = true;
      config = ''
        ;; Curate skill discovery rather than inheriting the upstream defaults
        ;; (~/.claude/skills etc.), so the exposed set is deterministic.
        (setopt gptel-agent-skill-dirs '("${skillDir}"))
        ;; MCP is intentionally not wired here (cf. enableMcpIntegration in the
        ;; other harnesses); built-in tools and sub-agents still apply.
        (gptel-agent-update)
      '';
    };
  };
}
