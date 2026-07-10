{ config, lib, ... }:
let
  cfg = config.harnesses;
in
{
  # memex deliberately bypasses programs.<harness>.skills: it's a live dev
  # checkout, and the skills options can't express that — they misclassify
  # mkOutOfStoreSymlink outputs (symlink, not directory => treated as a single
  # SKILL.md) and link directories with recursive=true, which snapshots entries
  # at activation instead of staying live. A plain top-level symlink keeps
  # edits in ~/projects/memex visible to all harnesses without a rebuild.
  home.file = lib.mkIf cfg.enable (
    let
      memexSkill.source = config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/projects/memex";
    in
    {
      ".claude/skills/memex" = memexSkill;
      ".codex/skills/memex" = memexSkill;
      ".config/opencode/skills/memex" = memexSkill;
      ".pi/agent/skills/memex" = memexSkill;
    }
  );
}
