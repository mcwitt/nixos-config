{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.profiles.base;
  # Claude (haiku, no tools/session) as the LLM for `wt` commit messages.
  # worktrunk's interactive `wt step commit` setup can't write our read-only
  # config, so we set it here. This is worktrunk's canonical Claude preset.
  # Bound as a normal Nix string so the empty-arg '' quotes don't collide with
  # the indented-string ('') escaping in the config below.
  claudeCommitCmd = "CLAUDECODE= MAX_THINKING_TOKENS=0 claude -p --no-session-persistence --model=haiku --tools='' --disable-slash-commands --setting-sources='' --system-prompt=''";
in
{
  config = lib.mkIf cfg.enable {
    home.packages = [ pkgs.worktrunk ];

    # `wt switch` needs a shell function to cd the parent shell. Install it via
    # the stdout `... | source` mode so nothing mutable is written to fish config.
    programs.fish.interactiveShellInit = ''
      ${lib.getExe pkgs.worktrunk} config shell init fish | source
    '';

    # User-global worktrunk config. Applies to ALL repos with no per-repo
    # approval prompt. Read-only Nix store symlink: change it here, not via
    # `wt config` (which would hit EROFS). Runtime state (activity markers,
    # per-branch vars) lives in git config, so it is unaffected.
    xdg.configFile."worktrunk/config.toml".text = ''
      # Sibling layout: ~/projects/<repo> stays put, worktrees become
      # ~/projects/<repo>.<branch>.
      worktree-path = "{{ repo_path }}/../{{ repo }}.{{ branch | sanitize }}"

      # Generate commit messages with Claude (for `wt step commit`, `wt merge`).
      [commit.generation]
      command = "${claudeCommitCmd}"

      [pre-start]
      # Freshen remote-tracking refs so new worktrees branch from an up-to-date base.
      fetch = "git fetch origin --prune"

      [post-start]
      # CoW-copy gitignored files (build dirs, untracked .env) from the primary.
      copy = "wt step copy-ignored"
      # Auto-allow direnv ONLY if the primary worktree's .envrc is already
      # trusted (allowed == 0) — never blindly trust a branch's .envrc.
      direnv = "( cd {{ primary_worktree_path }} && ${pkgs.direnv}/bin/direnv status --json | ${pkgs.jq}/bin/jq -e '.state.foundRC.allowed == 0' >/dev/null 2>&1 ) && ${pkgs.direnv}/bin/direnv allow || true"

      [post-merge]
      # After `wt merge main`, keep the primary main and origin in sync.
      sync = 'if [ "{{ target }}" = "main" ]; then git pull --ff-only && git push; fi'
    '';
  };
}
