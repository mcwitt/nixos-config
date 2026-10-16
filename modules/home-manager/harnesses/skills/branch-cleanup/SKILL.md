---
name: branch-cleanup
description: Find and remove stale local Git branches and worktrees in a repository — merged or squash-merged branches, branches with closed PRs or deleted upstreams, idle branches, and worktrees nobody is using — with a reviewed dry-run plan before anything is deleted. Use when the user asks to clean up, prune, or tidy local branches or worktrees. Not for deleting remote branches.
---

# Branch cleanup

A bundled script surveys the repository and writes a plan; a second command applies it. Run the script rather than re-deriving the rules by hand: it handles squash merges, rebased copies of commits, reflog timestamps and nested worktrees, which are easy to get wrong.

The script is `scripts/branch_cleanup.py` in this skill's directory. It needs Python ≥ 3.11 and Git ≥ 2.38, and uses the GitHub CLI for PR state when available.

## Workflow

1. **Plan.** Run `python3 <skill-dir>/scripts/branch_cleanup.py plan <repo>`, adding any options the user asked for (see below). It fetches with `--prune`, surveys every worktree and local branch, prints a summary and writes the plan to `<git-common-dir>/branch-cleanup/plan.json`. Nothing else changes.
2. **Present the plan.** Report the counts and the deletions, grouped by reason, and call out separately:
   - anything that loses commits existing only locally (the summary totals them);
   - worktrees with uncommitted changes or unique ignored files;
   - anything you are unsure about.
   Use `-v` to also list kept branches if the user wants to see them.
3. **Look for duplicates.** Among the kept branches, look for branches that largely duplicate a newer branch (similar names, the same PR rebased under another name, near-identical diffs against the base). The script does not delete these. List the pairs with evidence and ask the user before deleting any; delete confirmed ones with `git branch -D`, noting each name and SHA in your report so it can be restored.
4. **Confirm, then apply.** Ask before applying unless the user has already told you to proceed in this conversation. To exclude items, change their `"action"` to `"keep"` in the plan JSON; apply only acts on `"delete"` entries. Then run `python3 <skill-dir>/scripts/branch_cleanup.py apply <plan.json>`.
5. **Report.** Give the counts removed, anything skipped and why, and the log path. The log lists every deleted branch with its SHA; `git branch <name> <sha>` restores one until Git garbage-collects unreachable commits (two weeks by default). Uncommitted changes and untracked files in removed worktrees cannot be recovered.

`apply` re-surveys immediately before acting and only deletes items that the reviewed plan marked for deletion and that are still eligible with the same commit. Something that became active since the plan, or whose HEAD moved, is skipped and reported.

## Rules

The horizon defaults to 7 days.

**Worktrees** are never touched when they are the main worktree, locked, the working directory of a running process, had Git activity (checkout, commit, reset) in the last 24 hours, or contain a worktree that is kept. Entries whose directory is missing are pruned. Otherwise a worktree is removed when it is:

- clean: no uncommitted changes, no unique ignored files, and, if detached, no commits that exist only locally; or
- not clean, but with no activity within the horizon. Activity is the latest of the reflog entry time, HEAD changes, the HEAD commit, and the modification times of uncommitted and unique ignored files.

Ignored files only count when they are unique to the worktree: caches (`node_modules`, `.venv`, `__pycache__`, `target`, `result` and similar), configured noise patterns, and files identical to the main worktree's copy are disregarded, since worktree tools often copy ignored files into new worktrees. Unique files under configured *disposable* paths never block removal.

**Branches** are never deleted when they are the default branch or checked out in a kept worktree. A branch is stale when any of these hold:

- integrated into the base branch: an ancestor, every commit patch-equivalent to one on the base, or a simulated merge that adds nothing (covers squash merges);
- only a trivial diff (≤ 5 changed lines) remains against the base;
- its PR was merged (and the branch has nothing beyond the PR head) or closed;
- its upstream was deleted;
- no commits within the horizon, unless it has an open PR.

A stale branch is deleted when its commits all exist on the remote — in remote-tracking refs, fetched PR refs, PR heads known to the GitHub CLI, or as patch-equivalent rebased copies — or when it has had no commits or reflog activity within the horizon. A stale branch with recent local-only commits is kept.

## Options and configuration

`plan` accepts `--horizon DAYS`, `--remote NAME`, `--default-branch NAME`, `--disposable GLOB` (repeatable), `--gh COMMAND`, `--no-fetch`, `--refresh-prs` (the PR list is cached for an hour), `--out PATH` and `-v`.

Per-repository settings live in `<git-common-dir>/info/branch-cleanup.toml`, which is local and never committed. Command-line options override it. If the user wants settings to persist for a repository, write them there.

```toml
horizon_days = 7
remote = "origin"
default_branch = "main"        # default: the remote's HEAD
disposable = ["logs/", "wandb/"]  # unique ignored files here never block removal
noise = ["tests/snapshots/*/outputs/", "debug.log"]  # ignored entirely
gh = "gh"                      # GitHub CLI command, if the environment uses a wrapper
pr_limit = 10000
```

Patterns ending in `/` match a directory and everything under it; other patterns match the path or the file name.

## Notes

- Without PR state (no GitHub CLI, no access, or a non-GitHub remote) the script warns and relies on Git alone; closed-PR detection is then unavailable. Mention this when it happens.
- Repositories with many worktrees can take a minute to survey; most of it is `git status` and comparing ignored files.
- This skill only deletes local branches and worktrees. Do not delete remote branches or push as part of it.
