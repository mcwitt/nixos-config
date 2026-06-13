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
- **`scheduler-card` is version-capped until Home Assistant is upgraded — do NOT bump it past 4.0.10.** Newer releases require a newer HA core than our pinned nixpkgs provides (4.0.11+ needs ≥ 2026.1, 4.0.17+ needs ≥ 2026.4); on older cores the schedule popups render scrambled and unusable (upstream issue nielsfaber/scheduler-card#1130). Before any bump, compare the `homeassistant` key in the release's `hacs.json` against `nix eval --raw '.#nixosConfigurations.hob.config.services.home-assistant.package.version'`. When HA is finally new enough and the card is bumped, also try dropping the two build pins carried in our vendored `package.json` (typescript held at 5.8.3, picomatch held at 2.3.1 — see the comment in the package's `default.nix`).
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
- **`lexical-binding` cookie in the generated init wrappers** (`modules/home-manager/profiles/base/emacs/default.nix`, the `home.file` block): the `rycee.hmModules.emacs-init` module emits `~/.emacs.d/early-init.el` and `init.el` (thin wrappers that `require` the real `hm-{early-,}init.el`) without a `lexical-binding` cookie, which Emacs 31 warns about on every startup. We `lib.mkForce`-override both wrappers to add the cookie. Drop the override once the module adds it upstream — check the `home.file` block in `hm-modules/emacs-init.nix` of `nurNoPkgs.repos.rycee` (the wrappers don't currently carry the cookie even though the generated `hm-init.el` does).

### Home-manager modules to adopt when upstreamed

- **Shared agent skills** — track home-manager PR [#9247](https://github.com/nix-community/home-manager/pull/9247) (`agent-skills`: shared skills module for AI coding agents). When it lands, replace our per-harness skills injection (`gwsSkills`/`superpowersSkills` passed to each `programs.<harness>.skills` under `modules/home-manager/profiles/base/agents/`, renamed `harnesses/`) with the upstream shared module. Check status: `gh pr view 9247 --repo nix-community/home-manager`.
- **Writable codex `config.toml`** — track home-manager issue [#9397](https://github.com/nix-community/home-manager/issues/9397) (proposes `programs.codex.mutableUserSettings`, mirroring `programs.zed-editor`). codex (>= ~0.58, see [openai/codex#6646](https://github.com/openai/codex/issues/6646)) rewrites `~/.codex/config.toml` at startup — per-project `trust_level` (`config/batchWrite`) and `[tui.*]` counters — so the default read-only `/nix/store` symlink makes codex fail to start ("config/batchWrite failed in TUI"). Our workaround in `modules/home-manager/profiles/base/harnesses/codex/default.nix` disables the symlink (`home.file.".codex/config.toml".enable = lib.mkForce false`) and seeds a writable copy of the generated config via a `home.activation` script (cost: codex's runtime writes reset to the declarative baseline on each switch). When `mutableUserSettings` (or equivalent jq-merge) lands, drop the activation workaround and set the option. Check status: `gh issue view 9397 --repo nix-community/home-manager`.

### satori: WezTerm GPU crashes — mostly GPU-VRAM starvation from local LLMs (NVIDIA EGL)

`wezterm-gui` SIGSEGVs intermittently on satori (RTX 3090, X11), taking down every window at once (one GUI process hosts all windows). Recurring since April 2026 across multiple WezTerm builds, so it is not a single-WezTerm-version regression.

- **Fault signature:** SIGSEGV inside `libnvidia-eglcore.so` during `glium::ops::draw::draw` ← `wezterm_gui::termwindow::...::paint_impl`, preceded in the same session by `window::egl > make_current failed ... MakeCurrent: BAD_ALLOC` in the gui log. `EGL_BAD_ALLOC` is a GPU allocation failure (≈ VRAM OOM), not a WezTerm logic bug — but the proximate trigger is usually our own VRAM pressure, not a spontaneous driver fault (see below).
- **Two distinct causes (analysis 2026-06-04):**
  1. **Dominant / recent — VRAM exhaustion from a resident local LLM.** llama-swap keeps a large model loaded (e.g. `Qwen3.6-27B` ≈ 22 GiB; earlier `Qwen3-30B-A3B-Thinking-2507`) on a 24 GiB card, leaving the whole desktop (Xorg ~1.2 GiB + picom + browser + wezterm) fighting over a few hundred MiB. Measured once at **377 MiB free**. When wezterm needs GPU memory (`MakeCurrent`, a new glyph-atlas page, surface realloc on resize/DPI) and it doesn't fit, EGL returns `BAD_ALLOC`. It is **intermittent and usually survivable** — one session logged two `BAD_ALLOC`s 25 s apart and kept running for minutes — and crashes only on the occasional frame where a `glium` draw proceeds on the un-made-current context. This explains the monthly→weekly escalation: it tracks keeping a model resident.
  2. **Baseline — a rare EGL hiccup independent of VRAM.** The two earliest crashes (2026-04-06, 2026-04-07) happened *before any model was ever loaded* (first llama-swap request 2026-04-07 11:27; first model load 2026-04-08), so a low-rate driver/EGL flakiness exists on its own. VRAM pressure didn't create the bug; it turned this rare event into a frequent one.
- **Correlation method (to re-verify):** match `coredumpctl list | grep wezterm` crash times against periods when a model was resident. The `[INFO] Request ... POST /v1/...` access-log in `journalctl -u llama-swap` is the cleanest "model loaded" signal but was only verbose from late May; `<model> process exited` events are sparser but survive throughout and confirm the model was cycling. Live headroom: `nvidia-smi --query-gpu=memory.free --format=csv` and `nvidia-smi` process list. Of the 13 crashes to date: 6 provably model-loaded, 2 provably pre-LLM, 5 unprovable (no logged signal, none contradicting VRAM).
- **Fixes, in priority order:**
  - **Give the desktop VRAM headroom (addresses cause #1, the frequent one).** Configure llama-swap with an idle-unload TTL so the model isn't resident 24/7, and/or shrink ctx/KV or use a smaller quant; aim for ≥1–1.5 GiB free. Helps *every* GPU client, not just wezterm.
  - `front_end = "Software"` removes wezterm from the VRAM contest entirely (CPU render, negligible loss for a terminal) — now a justified trade given the cause. `front_end = "WebGpu"` is a lateral move on NVIDIA (own crash/lag reports) and was sluggish here; `prefer_egl = false` is a no-op on X11 (EGL is the only GPU path).
  - Upstream WezTerm robustness (skip the frame / recreate context on `make_current` `BAD_ALLOC` instead of crashing the whole GUI) would only mitigate the *rare* baseline cause #2; deprioritized.
- **Driver state:** NixOS default (`nvidiaPackages.stable`), no `hardware.nvidia.package` override; `hardware.nvidia.open = true`. As of 2026-06 our pinned nixpkgs still resolves stable/production/latest = 595.71.05 (beta older at 595.45.04). Upstream NVIDIA has since shipped 595.80 (point release) and 610.43.02 (new feature branch) — neither in our channels, and 610's notes mention no EGL/`MakeCurrent`/`BAD_ALLOC` fix, so a driver bump alone is unlikely to address cause #2. `open = false` won't help either — the fault is in userspace `libnvidia-eglcore.so`, identical across open/proprietary kernel modules.
- **Follow-up:** when a newer NVIDIA driver or WezTerm lands in our pinned nixpkgs, retest the baseline crash; if it recurs with a meaningfully different driver, consider pinning a known-good version via `hardware.nvidia.package = config.boot.kernelPackages.nvidiaPackages.mkDriver { ... }`. Check the current driver version with:
  ```bash
  nix eval --raw '.#nixosConfigurations.satori.config.boot.kernelPackages.nvidiaPackages.stable.version'
  ```

### TODO audit

```bash
grep -rn 'TODO\|FIXME\|HACK\|XXX' --include='*.nix' --include='*.el' .
```

### worktrunk + harness plugins

`pkgs.worktrunk` provides the `wt` CLI, the Claude Code plugin
(`${pkgs.worktrunk.src}` repo root), the Codex `worktrunk` skill
(`${pkgs.worktrunk.src}/skills/worktrunk`), and the OpenCode activity plugin
(`${pkgs.worktrunk.src}/dev/opencode-plugin.ts`). All three integrations are
sourced from the package's own `src`, so they cannot skew from the binary — bump
them together by bumping nixpkgs.

- Check the packaged version: `nix eval --raw 'nixpkgs#worktrunk.version'`.
- On a major bump, re-verify the Claude plugin path: v0.50.0 keeps the plugin at
  the repo root (`.claude-plugin/plugin.json`); newer `main` moved it under
  `plugins/worktrunk/`. If `${pkgs.worktrunk.src}/.claude-plugin/plugin.json`
  disappears, repoint the `plugins` entry in
  `harnesses/claude-code/default.nix`.
- worktree path template and lifecycle hooks live in
  `modules/home-manager/profiles/base/worktrunk.nix` (read-only at
  `~/.config/worktrunk/config.toml` — edit the Nix module, not the file).

## Conventions

- Package upgrade commit subjects: `<package-path>: <old-version> -> <new-version>` (e.g. `python3Packages.gehomesdk: 2025.11.5 -> 2026.2.0`).
- Risky or potentially-reverted changes (workaround removals, fork bumps) go in their own commit for easy rollback.
- Pre-commit hook is `nixfmt-rfc-style`; `hardware-configuration.nix` is excluded.
