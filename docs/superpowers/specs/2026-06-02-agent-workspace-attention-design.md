# Agent workspace-attention indicators — Design

Status: draft for review
Date: 2026-06-02

## Problem

The project-focused dynamic xmonad workspaces (commit `038eee3f`) make it easy to
run several Claude Code agents, one per project workspace. But there is no
glanceable way to know *which* workspaces have an agent that needs feedback, so
the user keeps switching workspaces to check on progress. The existing
`peon-ping` (audible, on the Claude Code `Notification` hook) is ineffective:
sound is usually muted (family), and even unmuted it does not localize *which*
workspace/window wants attention.

## Goal

A silent, glanceable, **per-workspace** indicator of which workspaces have an
agent that needs feedback — reusing the urgency machinery xmonad already has
(the 🔔 / `base0A` urgent-workspace block in polybar, and `M-S-u`). Plus an
instant locator (a dunst popup naming the workspace) for the moment it happens.

## Non-goals

- **Running/busy state.** Only "needs feedback" is surfaced (resolved in
  brainstorming).
- **Remote/SSH agents.** The hook runs where Claude runs; a local X session is
  assumed.
- **Window-level precision.** Attribution is workspace-level (see Constraint).
- Click-to-jump dunst actions, per-source colors, a `Notification`-hook trigger,
  and an idle refresh timer — all deferred (YAGNI).

## Constraint: why attribution is cwd-based

When the `Stop` hook fires, the hook process (a child of Claude Code, inside a
wezterm pane) **cannot learn which xmonad workspace it is in.** Determining the
workspace would require the process's X11 window id (→ EWMH `_NET_WM_DESKTOP` →
`_NET_DESKTOP_NAMES`), and that id is unobtainable from inside a wezterm pane:

- wezterm does **not** set `WINDOWID` (confirmed on the live system).
- `wezterm cli list` exposes only multiplexer ids (`window_id`/`tab_id`/
  `pane_id`), never the X11 id; the maintainer has stated they "will never
  support specifically exposing X window IDs"
  (<https://github.com/wezterm/wezterm/discussions/3971>).
- wezterm runs **all** windows in one process, so matching `_NET_WM_PID` returns
  every wezterm window indistinguishably (verified live: 5 windows, 1 PID).
- The window **title** does leak through (`_NET_WM_NAME` = active pane title),
  but title-search only works when the agent's pane is the active pane of the
  active tab, is intrusive, and is defeated by the changing status-glyph prefix.
  Not robust.

Therefore **cwd is the only workspace-correlated signal the hook has.** The hook
records the cwd; xmonad attributes it to a workspace by path convention. This
works for path-resolvable (project) workspaces; the `--workspace` escape hatch
(below) covers anything else.

## Decisions (from brainstorming)

- **Trigger:** Claude Code `Stop` hook, top-level only (excludes
  `SubagentStop`). A `Notification`-hook trigger is a possible future toggle.
- **Approach:** fold agent-attention into xmonad's existing urgent-workspace
  strip rather than a separate polybar module.
- **Push refresh:** `XMonad.Hooks.ServerMode` (`serverModeEventHookCmd'` with a
  fixed one-command list) + `pkgs.xmonadctl` as the client. Already-available
  server side via `enableContribAndExtras = true`; `xmonadctl` is packaged in
  nixpkgs (0.18.2).
- **Notification owner:** xmonad fires the dunst popup — so it can suppress the
  popup for any workspace currently visible, and title it with the real tag.
- **Jump:** generalize `M-S-u` to step through window-urgent **and**
  agent-attention workspaces.
- **State:** `$XDG_RUNTIME_DIR/workspace-attention/` (tmpfs; auto-cleared on
  logout/reboot; survives an xmonad `--restart`).
- **Producer:** a generic `workspace-attention` CLI; the Claude `Stop` hook is
  thin glue. peon-ping is left untouched (complementary).

## Architecture

```
Claude Code (top-level turn ends)
   │  Stop hook, JSON on stdin (incl. cwd)
   ▼
Claude Stop glue  (jq .cwd)  ── gated on profiles.desktop.enable
   ▼
workspace-attention CLI  (generic producer; single concern)
   ├─ write per-key state file in $XDG_RUNTIME_DIR/workspace-attention/
   └─ xmonadctl agent-attention-refresh   (|| true)
   ▼
xmonad (consumer)
   ├─ serverModeEventHookCmd' "agent-attention-refresh" -> refresh
   └─ logHook:
        ├─ attribute each state file -> workspace tag
        ├─ clear-on-view: delete files for any *visible* tag
        ├─ popup (xmonad): notify-send for newly-attributed, non-visible tags
        └─ render: union agent tags into the existing 🔔/base0A urgent block
   ▼
polybar bell  +  M-S-u jumps to the next workspace needing you
```

Two channels by design: the **dunst popup** is the instant "something needs you,
here's where" (push, names the workspace, silent); the **polybar strip** is the
persistent "what's still pending" glance + the `M-S-u` target.

### Component 1 — `workspace-attention` CLI (generic producer)

Interface:

```
workspace-attention [--source S] [--message M] [--workspace TAG] [DIR]
```

- `DIR` defaults to `$PWD`.
- Identifier: `tag=<TAG>` when `--workspace` is given (verbatim, skips
  attribution); otherwise `cwd=<realpath DIR>`.
- `key = sha1(<identifier line>)` — dedups repeat-`Stop`s to one file and
  sidesteps `/` or `:` in tags.
- Directory: `$XDG_RUNTIME_DIR/workspace-attention/` (`mkdir -p`).
- Write only if neither `$dir/$key` nor `$dir/$key.seen` exists (one popup per
  attention episode). Atomic write (temp file + `mv`).
- Poke: `xmonadctl agent-attention-refresh` with failure ignored (`|| true`), so
  it is a no-op when xmonad is absent/restarting.
- Defaults: `source=agent`, `message=needs attention`.
- Built as `pkgs.writeShellApplication`; `runtimeInputs = [ coreutils
  xmonadctl ]` (for `sha1sum`/`realpath`/`mktemp`/`mv` and the poke).
- **Single concern:** record attention + wake xmonad. It does **not** compute a
  tag (beyond passing `--workspace` through) and does **not** fire dunst.

### Component 2 — Claude Code `Stop` hook (glue)

- Added to `programs.claude-code.settings.hooks.Stop`, **gated on
  `config.profiles.desktop.enable`** (so it is not installed on headless hosts),
  mirroring how the peon hook is gated on `programs.peon-ping.enable`.
- A small script: read stdin JSON, extract `.cwd` (`jq`), call
  `workspace-attention --source claude --message "agent finished" "<cwd>"`.
- Referenced by absolute Nix store path (like the existing `peon` hook); never
  via `PATH`.
- **Verify during implementation:** the `Stop` hook stdin payload field is
  `cwd` for the installed Claude Code version (payload also carries
  `session_id`, `transcript_path`, `hook_event_name`, `stop_hook_active`).
- peon-ping (on `Notification`) is unchanged; this is additive.

### Component 3 — xmonad (consumer), in `xmonad.hs`

Shared literals with the CLI (cross-referenced by comment, not Nix-substituted):
the state-dir subpath `workspace-attention` under `$XDG_RUNTIME_DIR` (read via
`getEnv "XDG_RUNTIME_DIR"` at runtime) and the command name
`agent-attention-refresh`.

**a. Attention model**

- `readAttention :: X [Entry]` reads the state dir; each file →
  `Entry { file, ident (cwd or tag), source, message, notified (name ends
  ".seen") }`. Guarded with the existing `safeIO` pattern.
- `attribute :: Entry -> X (Maybe Tag)`:
  - explicit `tag=` → `Just tag` iff `tag` is an existing workspace
    (`existingTags`), else `Nothing`;
  - `cwd=` → the **longest** existing tag whose resolved path is a
    path-component ancestor of the cwd. A `p:k` tag resolves to
    `<projects-root> </> k` (string only — reuse the `namespaces` roots; **no**
    marker walk, no filesystem IO).
- `visibleTags = currentTag : map (W.tag . W.workspace) visible` (current +
  any workspace shown on another screen).

**b. logHook integration** (inside `mkPolybarLogHook`)

For each attention entry, with attributed tag `t`:

- `t ∈ visibleTags` → **clear-on-view**: delete the file (both `<key>` and
  `<key>.seen` forms) and skip it.
- `t ∉ visibleTags` →
  - add `t` to the agent-attention tag set;
  - if the entry is **not** notified → fire `notify-send -a <source> "<t>"
    "<message>"` (via the substituted `notifySend`), then rename the file to
    `<key>.seen`.
- `Nothing` (unattributable; e.g. cwd outside any path namespace) → leave it
  (renders nowhere; reaped on logout via tmpfs).

`urgentExtras` (`xmonad.hs:209`) is extended to render the **union** of the
agent-attention tag set and the existing `readUrgents`-derived urgent tags
(minus `visibleTags` and the scratchpad), through the **same** `highlight
base0A` + bell glyph. Window-urgency rendering is unified onto `visibleTags`
too for consistency.

**c. handleEventHook** — add:

```haskell
serverModeEventHookCmd' (return [("agent-attention-refresh", refresh)])
```

(`refresh = windows id` re-runs the configured logHook; the relayout is
idempotent/invisible. A no-relayout variant — re-running just the log action — is
an equivalent alternative.)

**d. Keybinding** — replace `("M-S-u", focusUrgent)` with `("M-S-u",
focusAttention)`:

- `focusAttention` gathers urgent windows (`readUrgents`) and agent-attention
  tags (attributed, `∉ visibleTags`), and steps to the next attention target
  (≠ current). For an urgent-window target it focuses that **window**
  (`focusUrgent` semantics); for an agent-only tag it `greedyView`s the
  **workspace**. No-op when nothing needs attention. (Exact cycle implementation
  deferred to the plan; the behavior is "one key cycles through everything
  needing attention.")
- **Precision is per-source, by necessity.** Agent jumps are workspace-level,
  not window-level — we only ever have a cwd→workspace inference, never the
  agent's window handle (the wezterm constraint above). In practice `greedyView`
  restores the workspace's last-focused window, which is usually the agent
  terminal; `M-o` (EasyMotion) finishes the hop otherwise. True window-level
  focus would require running agents in an urgency-on-bell terminal
  (`xterm`/`urxvt`), sacrificing wezterm — explicitly rejected.

**e.** `home.packages += pkgs.xmonadctl`.

No `startupHook` is needed — `$XDG_RUNTIME_DIR` already gives the right
lifecycle.

### State file / directory format

```
$XDG_RUNTIME_DIR/workspace-attention/
├── <sha1>            # pending  (popup not yet fired)
└── <sha1>.seen       # notified (popup fired; bell still showing)
```

File content (key=value, atomic write):

```
cwd=/home/matt/projects/nixos-config/modules/x     # or:  tag=scratch
source=claude
message=agent finished
```

## Edge cases

| # | Case | Handling |
|---|------|----------|
| 1 | Agent finishes while you're **viewing** its workspace (or it's visible on another monitor) | logHook sees the tag is in `visibleTags` → no bell (clear-on-view) and no popup. The "unhelpful ping" failure mode is designed out. |
| 2 | **Rapid successive Stops** while away | CLI writes only if absent (`sha1(cwd)`); already-pending → just re-poke. One popup per episode; a new one only after you view & clear it. |
| 3 | **Multiple agents, different workspaces** | One file per cwd → multiple bells, each cleared on its own view. |
| 4 | **Multiple agents, same workspace** (sub-dirs) | Both attribute to one tag → one bell, cleared together. |
| 5 | xmonad **`--restart`** with pending attention | Files survive in `$XDG_RUNTIME_DIR`; `.seen` flag (filesystem, not in-memory) prevents re-notify spam. |
| 6 | **Reboot / logout** | tmpfs → state gone → no stale bells. No cleanup code. |
| 7 | Agent run **outside any path namespace** | No tag attributes → no bell, no popup (documented limitation; `--workspace` is the escape hatch). |
| 8 | **Workspace renamed** away from its project | cwd attributes to `p:<project>`; if not an existing tag → no bell (never a *wrong* bell). |
| 9 | **xmonad not running / mid-restart** when hook fires | CLI ignores `xmonadctl` failure; file written, renders on next refresh. |
| 10 | Deps not on the hook's `PATH` | CLI, `xmonadctl`, `notify-send` referenced by absolute store path / pinned `runtimeInputs`. |
| 11 | **Non-desktop host** | Whole feature gated on `profiles.desktop.enable`; hook not installed. |
| 12 | Tags with `/` or `:` | Files keyed by `sha1`; tags are opaque strings everywhere. |
| 13 | **Read/write race** | CLI writes atomically (temp + `mv`); worst case one stale read, fixed by next poke. |
| 14 | Autonomous **`/loop`** session | `Stop` fires per iteration → bells each loop; disable the hook for such sessions (out of scope). |
| 15 | **logHook perf** | tmpfs dir, 0–3 tiny files → microseconds; `safeIO`-guarded. |

## Generality / extension

The producer is app-agnostic; only the per-app glue is specific.

- **Other agents:** Codex's `notify` program, aider, etc. → call
  `workspace-attention` with an appropriate `--source`/`--message`.
- **Manual:** `long-build && workspace-attention --message "build done"`.
- **Non-project / explicit:** `workspace-attention --workspace <tag>`.
- **More path namespaces:** extend xmonad's `namespaces`; attribution covers
  them automatically.

## Security

`serverModeEventHookCmd'` is used with a fixed one-command list, so no
arbitrary-command execution is exposed to local X clients (unlike the broader
`serverModeEventHook`).

## Testing

- **CLI** (temp `XDG_RUNTIME_DIR`, a `PATH`-shimmed `xmonadctl`): file created
  with the right name/content; idempotent on repeat; `--workspace` path; poke
  invoked.
- **Claude glue:** feed sample `Stop` JSON on stdin → asserts the CLI is called
  with the extracted cwd.
- **xmonad:** `nix flake check`; build the home-manager xmonad config; manual
  smoke matrix — finish an agent in a non-visible workspace (bell + one popup
  titled with the tag); view it (bell clears, file gone); finish in a visible
  workspace (silent); two agents (two bells, `M-S-u` cycles); logout (no stale
  bells).
- `nixfmt-rfc-style` pre-commit; commit-subject conventions.

## Open questions / future

- Static vs. dynamic `Stop` message (e.g. "finished" vs. "asked a question").
- Optional `Notification`-hook trigger toggle (mid-turn permission/idle waits).
- Optional dunst action to click-jump to the workspace.
- **Urgent-block contrast fix (pre-existing).** The bell highlight
  (`xmonad.hs:244`, `highlight bg = ... %{F<base01>} ...`) draws foreground
  `base01` on background `base0A` (yellow); `base01` is a near-background shade,
  so the text is hard to read. Pick a higher-contrast foreground for the yellow
  block (e.g. `base00`, or polarity-aware). The agent-attention bells reuse this
  exact `highlight`, so fixing it benefits both — worth folding into this work.

## Repo change list

- **new** (desktop profile, near xmonad): `workspace-attention` CLI package,
  exposed for the Claude Code hook to reference by store path.
- `modules/home-manager/profiles/desktop/xmonad/xmonad.hs`: attention model,
  logHook integration, `serverModeEventHookCmd'`, generalized `M-S-u`.
- desktop profile: `home.packages += [ pkgs.xmonadctl <cli> ]`.
- `modules/home-manager/profiles/base/harnesses/claude-code/default.nix`: the
  `Stop` hook (gated on `profiles.desktop.enable`), referencing the CLI.
