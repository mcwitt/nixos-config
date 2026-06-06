{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.profiles.base;
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

      [pre-start]
      # Freshen remote-tracking refs so new worktrees branch from an up-to-date base.
      fetch = "git fetch origin --prune"

      [post-start]
      # CoW-copy gitignored files (build dirs, untracked .env) from the primary.
      copy = "wt step copy-ignored"
      # direnv works immediately in the new worktree (nix-direnv is enabled).
      direnv = "direnv allow"

      [post-merge]
      # After `wt merge main`, keep the primary main and origin in sync.
      sync = 'if [ "{{ target }}" = "main" ]; then git pull --ff-only && git push; fi'
    '';
  };
}
