# AGENTS.md

This file provides guidance to AI coding agents (Claude Code, Codex CLI, Aider, etc.) when working with code in this repository. It follows the [agents.md](https://agents.md/) convention. `CLAUDE.md` is a symlink to this file.

## Overview

Personal NixOS configurations and nixpkgs overlays. Flake-based, multi-host.

## Build and Development Commands

### Testing Changes
```bash
# Check flake evaluates and run pre-commit checks
nix flake check

# Build a specific NixOS configuration
nixos-rebuild build --flake .#<hostname>
```

### Applying Changes
```bash
# Switch current host to new configuration
sudo nixos-rebuild switch --flake .

# Deploy to remote host (wraps nixos-rebuild switch with --target-host root@$1 --build-host localhost)
./deploy.sh <hostname>
```

### Development
```bash
# Enter dev shell (sets up the pre-commit hook)
nix develop

# Format Nix files manually (the pre-commit hook does this on commit)
nixfmt <file.nix>
```

## Architecture

The repo is laid out roughly as `flake.nix` + `modules/` (reusable NixOS / home-manager modules) + `hosts/` (per-host configs) + `overlay/` + `packages/`. Browse those directories for the concrete shape — what follows is just the non-obvious patterns.

### Profiles system (opt-in)

Hosts don't import modules directly; they enable feature bundles via `profiles.<name>.enable = true;`. Both the NixOS side (`modules/nixos/profiles/`) and the home-manager side (`modules/home-manager/profiles/`) define their own set under the `profiles.*` namespace. Read those directories to see what's available — each profile is a single `default.nix` that declares its own `mkEnableOption`.

Hosts compose profiles in `flake.nix` under their `nixosConfigurations.<host>` entry; the home-manager profiles are set under `home-manager.users.<user>.profiles`.

### Host wiring

Two patterns, visible in `flake.nix`:

- **`self.lib.makeNixosSystem`** (golem, karakuri, satori): helper that wires home-manager, overlays, stylix, and the default modules. Use this for any new desktop host.
- **`nixpkgs.lib.nixosSystem` directly** (hal, hestia, hob): used when a host needs to skip home-manager or use a different `nixosSystem` builder (hob uses `nixos-raspberrypi.lib.nixosSystem` and cross-compiles from x86_64).

### The private config

A second repo at `~/projects/nixos-config-private` extends the public host configurations via `overrideNixosSystem` / `.override`. Before deleting anything that looks unused (overlay entries, profile options, package definitions), grep there:

```bash
grep -r "<symbol>" ~/projects/nixos-config-private/
```

## Periodic Maintenance

These are the non-obvious things that need attention over time. The code can't tell you when they've gone stale.

### Stale overrides in the overlay

- **Home Assistant custom components and lovelace modules** (under `packages/servers/home-assistant/`): check upstream releases with `gh release list --repo <owner/repo> --limit 1`. For `scheduler-card`, updating requires regenerating `package-lock.json` (`npm install --package-lock-only` on the new source, then `prefetch-npm-deps` for the hash).
- **`magicattr`** (`packages/development/python-modules/magicattr/`): not in nixpkgs. Check periodically with `nix eval 'nixpkgs#python3Packages.magicattr.version'` to see if it's been upstreamed.

### Useful version-checking commands

```bash
# Latest version of a package in nixpkgs (will fail at build time if our override is stale)
nix eval --raw 'nixpkgs#python3Packages.<pkg>.version'

# Latest GitHub release
gh release list --repo <owner/repo> --limit 1

# Prefetch a fetchFromGitHub source hash
nix store prefetch-file --json --hash-type sha256 "https://github.com/<owner>/<repo>/archive/refs/tags/<tag>.tar.gz"

# Prefetch a PyPI source hash
nix store prefetch-file --json --hash-type sha256 "https://files.pythonhosted.org/packages/source/<first-letter>/<pkg>/<pkg>-<version>.tar.gz"
```

### Emacs workarounds to retire eventually

- **`org-latex-preview`** (`modules/home-manager/profiles/base/emacs/org-latex-preview.nix`): pulls org-mode from the tecosaur fork (`code.tecosaur.net/tec/org-mode`, `dev` branch) for the org-latex-preview overhaul. Not yet merged into mainline org-mode — see the [org-mode mailing list thread](https://list.orgmode.org/orgmode/87lek2up0w.fsf@tec.tecosaur.net/). When it lands, drop the fork override and keep just the `usePackage.org` config. To bump the fork: `nix-prefetch-git --url https://code.tecosaur.net/tec/org-mode.git --rev dev --quiet`.
- **`citar` + native-comp**: previously needed `packageQuickstart = lib.mkForce false` plus `(package-activate-all)` in the prelude due to a `citar-indicator` type error. Workaround removed; if it comes back, see git history.

### Home-manager modules to adopt when upstreamed

- **Shared agent skills** — track home-manager PR [#9247](https://github.com/nix-community/home-manager/pull/9247) (`agent-skills`: shared skills module for AI coding agents). When it lands, replace our per-harness skills injection (`gwsSkills`/`superpowersSkills` passed to each `programs.<harness>.skills` under `modules/home-manager/profiles/base/agents/`, renamed `harnesses/`) with the upstream shared module. Check status: `gh pr view 9247 --repo nix-community/home-manager`.

### TODO audit

```bash
grep -rn 'TODO\|FIXME\|HACK\|XXX' --include='*.nix' --include='*.el' .
```

## Conventions

- Package upgrade commit subjects: `<package-path>: <old-version> -> <new-version>` (e.g. `python3Packages.gehomesdk: 2025.11.5 -> 2026.2.0`).
- Risky or potentially-reverted changes (workaround removals, fork bumps) go in their own commit for easy rollback.
- Pre-commit hook is `nixfmt-rfc-style`; `hardware-configuration.nix` is excluded.
