---
name: maintain
description: Run periodic maintenance on the NixOS config. Checks for stale package overrides, dead code, and TODO/FIXME comments.
disable-model-invocation: true
---

Run periodic maintenance on this NixOS configuration. Work through each section below, report findings, then ask which items to act on before making changes.

## 1. Update flake inputs

Run `nix flake update` to update flake.lock, then `nix flake check` to verify. Commit as `Update flake.lock` if successful. If the check fails, investigate which input caused the breakage — try updating inputs individually with `nix flake lock --update-input <input-name>` to isolate the problem.

## 2. Deprecation warnings

Run `nix flake check 2>&1` and look for deprecation warnings (lines containing `warning:`, `deprecated`, `is renamed to`, `evaluation warning`, etc.) in the output. For each warning:

- Identify the offending file/option from the trace.
- Determine the recommended replacement (usually stated in the warning text, or findable in the upstream module/option docs).
- Flag for action. Most are mechanical option renames; some may require restructuring.

## 3. Stale package overrides

Enumerate the overrides currently in `overlay/default.nix`. For each one, determine its type and check accordingly:

- **Version bumps of nixpkgs packages** (e.g., a Python package pinned to a newer release): compare the overlay version to nixpkgs (`nix eval --raw 'nixpkgs#<attr-path>.version'`) and to the upstream source (PyPI, GitHub releases). If nixpkgs has caught up, the override can be removed; if upstream is ahead, it can be bumped.
- **Custom packages not in nixpkgs** (e.g., something defined entirely in the overlay): check whether it has since been added to nixpkgs (`nix eval --raw 'nixpkgs#<attr-path>.version'`). If so, the custom definition can be removed.
- **Platform-specific patches**: check whether any host actually targets that platform. If not, flag as inert.

## 4. Home Assistant custom components and lovelace modules

Enumerate the packages under `packages/servers/home-assistant/` (don't rely on a hardcoded list). For each, find the upstream repo (usually in a `src = fetchFromGitHub { ... }` block) and check the latest release:

```
gh release list --repo <owner>/<repo> --limit 1
gh repo view <owner>/<repo> --json isArchived,pushedAt
```

Compare to the version pinned in the corresponding `.nix` file. Flag any that are behind, archived, or inactive.

## 5. Workarounds

Look for workarounds anywhere in the tree and flag any whose upstream tracking issue may have been resolved. Typical patterns to watch for:

- Pins to a fork or non-default branch (check whether the upstream change has landed in mainline).
- Disabled features or `mkForce` overrides with a comment referencing a bug (check whether the bug is still open).
- Inline patches (`patches = [ ./foo.patch ]`) — check whether the patched-around issue has been fixed upstream.

## 6. Dead code

Search for potentially unused overlay entries and modules:

- For each attribute in `overlay/default.nix`, check if it's referenced in any module, host config, or in `~/projects/nixos-config-private/`.
- For each optional module (`tools.*.enable`, etc.), check if any host or profile actually enables it.
- Check for modules that are defined but never imported.

## 7. TODO/FIXME/HACK/XXX audit

```
grep -rn 'TODO\|FIXME\|HACK\|XXX' --include='*.nix' .
```

For each match, determine if the upstream issue has been resolved. Provide links to the relevant issues/PRs where possible.

## 8. Upstream sanity check

When a version bump is on the table (sections 3 and 4), skim the upstream changelog and commit log between the pinned version and the proposed version. A full audit is impractical — the goal is a quick smell test, not a security review. Flag anything that looks off.

**Supply-chain smell signals:**

- New maintainers or a sudden shift in commit authorship.
- Obfuscated, minified, or unusually large blobs added (especially in non-build paths).
- New network endpoints, credential reads, or `eval`/`exec`-style dynamic execution.
- Changes to install/postinstall scripts, CI pipelines, or release tooling.
- Long stretches of inactivity followed by a sudden release.
- Releases that don't match the commit history (e.g., tag points at a tree that wasn't reviewed).

**Breakage signals:**

- Explicit "BREAKING" / "breaking change" notes in the changelog.
- Renamed or removed config options, attributes, or module entry points used by this repo.
- Dropped support for a NixOS / Home Assistant / Python / etc. version still in use here.
- Default behavior changes that would silently alter runtime semantics (auth, storage paths, ports).
- Schema or on-disk format migrations (especially one-way).
- Dependency bumps that may conflict with what nixpkgs provides.

Apply more scrutiny to less-active, single-maintainer, or otherwise lower-trust repositories. Surface any findings in the report and let the user decide whether to proceed.

## Reporting

Present findings as a structured report with clear categories:

- **Action needed**: stale versions, resolved workarounds, confirmed dead code
- **Informational**: unmerged upstream work, inactive but not archived repos
- **No action**: items that are current

Then ask the user which items to proceed with before making any changes.

## Making changes

- Each package upgrade gets its own commit: `<package-path>: <old> -> <new>`
- Risky changes (workaround removals, fork updates) get separate commits
- Use `nix store prefetch-file` or `nix-prefetch-git` for new hashes
- For scheduler-card updates, regenerate `package-lock.json` and `npmDepsHash`
- Run `nix flake check` after all changes to validate
- Format changed files with `nixfmt`
