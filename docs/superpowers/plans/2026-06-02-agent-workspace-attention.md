# Agent Workspace-Attention Indicator — Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.
>
> **Commit policy for this repo:** the repo owner requires explicit approval before any `git commit`. The commit steps below are the intended end-state of each task; when executing, *ask the owner to confirm* before running them. Commit subjects follow the repo convention; risky/standalone changes get their own commit.

**Goal:** When a Claude Code agent finishes its turn (`Stop`) in a project workspace, mark that workspace as needing attention — a 🔔 entry in polybar, a dunst popup naming the workspace, and a `M-S-u` jump target — reusing xmonad's existing urgency machinery.

**Architecture:** A Claude `Stop` hook records the agent's `cwd` (the only workspace-correlated signal available — wezterm exposes no window id) into a tmpfs state dir via a generic `workspace-attention` CLI, then wakes xmonad with `xmonadctl`. xmonad's logHook attributes each cwd to an existing workspace tag by path prefix, renders off-screen ones in the existing urgent block, fires a dunst popup for newly-attributed non-visible workspaces, and clears markers on view. `M-S-u` is generalized to step through window-urgent and agent-attention workspaces.

**Tech Stack:** NixOS + home-manager modules (Nix), xmonad config (`xmonad.hs`, Haskell, substituted by Nix; `xmonad-contrib` available via `enableContribAndExtras`), `pkgs.xmonadctl` + `XMonad.Hooks.ServerMode`, POSIX shell (`writeShellApplication`), `bats` for shell tests, `dunst`/`notify-send`.

**Spec:** `docs/superpowers/specs/2026-06-02-agent-workspace-attention-design.md`

---

## File Structure

**Create:**
- `modules/home-manager/profiles/desktop/xmonad/workspace-attention.sh` — generic producer CLI (records a cwd/tag + wakes xmonad). One concern.
- `modules/home-manager/profiles/desktop/xmonad/workspace-attention.bats` — tests for the CLI.
- `modules/home-manager/profiles/desktop/workspace-attention.nix` — wraps the `.sh` as a package, declares `profiles.desktop.workspaceAttention.package`, installs it + `pkgs.xmonadctl`.
- `modules/home-manager/profiles/base/harnesses/claude-code/claude-stop-attention.sh` — `Stop`-hook glue (`jq .cwd` → CLI).
- `modules/home-manager/profiles/base/harnesses/claude-code/claude-stop-attention.bats` — tests for the glue.

**Modify:**
- `modules/home-manager/profiles/desktop/xmonad/xmonad.hs` — attention model, logHook integration, contrast fix, ServerMode command, generalized `M-S-u`.
- `modules/home-manager/profiles/desktop/default.nix` — import `./workspace-attention.nix`.
- `modules/home-manager/profiles/base/harnesses/claude-code/default.nix` — add the `Stop` hook (gated on `profiles.desktop.enable`).

**Shared constants** (kept in sync by comment cross-reference, not Nix-substituted): the state-dir subpath `workspace-attention` under `$XDG_RUNTIME_DIR`, and the ServerMode command name `agent-attention-refresh`.

**Gating consistency note:** the existing `claude-code/default.nix` already reads `config.programs.peon-ping.enable` (an option declared by a desktop-imported module) — proving all home-manager profile modules are imported on every home-manager host, so `config.profiles.desktop.enable` and `config.profiles.desktop.workspaceAttention.package` are always in scope. This plan mirrors that pattern.

---

## Task 1: The `workspace-attention` CLI + tests

**Files:**
- Create: `modules/home-manager/profiles/desktop/xmonad/workspace-attention.sh`
- Test: `modules/home-manager/profiles/desktop/xmonad/workspace-attention.bats`

- [ ] **Step 1: Write the failing test**

Create `modules/home-manager/profiles/desktop/xmonad/workspace-attention.bats`:

```bash
#!/usr/bin/env bats
# Run: nix run nixpkgs#bats -- modules/home-manager/profiles/desktop/xmonad/workspace-attention.bats

setup() {
  export XDG_RUNTIME_DIR="$BATS_TEST_TMPDIR/run"
  mkdir -p "$XDG_RUNTIME_DIR"
  STUBBIN="$BATS_TEST_TMPDIR/bin"
  mkdir -p "$STUBBIN"
  cat > "$STUBBIN/xmonadctl" <<'EOF'
#!/usr/bin/env bash
echo "$@" >> "$XDG_RUNTIME_DIR/xmonadctl.calls"
EOF
  chmod +x "$STUBBIN/xmonadctl"
  export PATH="$STUBBIN:$PATH"
  SCRIPT="${BATS_TEST_DIRNAME}/workspace-attention.sh"
  STATE="$XDG_RUNTIME_DIR/workspace-attention"
}

@test "cwd form writes one state file with cwd=/source=/message= content" {
  mkdir -p "$BATS_TEST_TMPDIR/proj"
  run bash "$SCRIPT" --source claude --message "done" "$BATS_TEST_TMPDIR/proj"
  [ "$status" -eq 0 ]
  [ "$(ls -1 "$STATE" | wc -l)" -eq 1 ]
  f="$(ls "$STATE")"
  grep -qx "cwd=$(realpath "$BATS_TEST_TMPDIR/proj")" "$STATE/$f"
  grep -qx "source=claude" "$STATE/$f"
  grep -qx "message=done" "$STATE/$f"
}

@test "repeat call is idempotent (one file) but pokes each time" {
  mkdir -p "$BATS_TEST_TMPDIR/proj"
  bash "$SCRIPT" "$BATS_TEST_TMPDIR/proj"
  bash "$SCRIPT" "$BATS_TEST_TMPDIR/proj"
  [ "$(ls -1 "$STATE" | wc -l)" -eq 1 ]
  [ "$(wc -l < "$XDG_RUNTIME_DIR/xmonadctl.calls")" -eq 2 ]
}

@test "does not recreate a pending file once it has been renamed to .seen" {
  mkdir -p "$BATS_TEST_TMPDIR/proj"
  bash "$SCRIPT" "$BATS_TEST_TMPDIR/proj"
  f="$(ls "$STATE")"
  mv "$STATE/$f" "$STATE/$f.seen"
  bash "$SCRIPT" "$BATS_TEST_TMPDIR/proj"
  [ "$(ls "$STATE")" = "$f.seen" ]
}

@test "--workspace writes tag= content (no cwd attribution)" {
  run bash "$SCRIPT" --workspace scratch
  [ "$status" -eq 0 ]
  f="$(ls "$STATE")"
  grep -qx "tag=scratch" "$STATE/$f"
}

@test "missing XDG_RUNTIME_DIR fails" {
  unset XDG_RUNTIME_DIR
  run bash "$SCRIPT" --workspace x
  [ "$status" -ne 0 ]
}
```

- [ ] **Step 2: Run the test to verify it fails**

Run: `nix run nixpkgs#bats -- modules/home-manager/profiles/desktop/xmonad/workspace-attention.bats`
Expected: FAIL — every test errors because `workspace-attention.sh` does not exist.

- [ ] **Step 3: Write the CLI**

Create `modules/home-manager/profiles/desktop/xmonad/workspace-attention.sh`:

```bash
# workspace-attention — mark the workspace owning a directory (or an explicit
# tag) as needing attention, for the xmonad agent-attention indicator.
#
# The consumer is xmonad.hs: it reads $XDG_RUNTIME_DIR/workspace-attention/ and
# renders/clears bells. This script only RECORDS attention and WAKES xmonad; it
# does not compute a workspace tag (xmonad attributes the cwd) and does not fire
# any popup (xmonad owns that). Keep the "workspace-attention" subdir name and
# the "agent-attention-refresh" command in sync with xmonad.hs.
#
# Usage: workspace-attention [--source S] [--message M] [--workspace TAG] [DIR]
set -euo pipefail

source="agent"
message="needs attention"
tag=""
dir=""

while [ $# -gt 0 ]; do
  case "$1" in
    --source) source="$2"; shift 2 ;;
    --message) message="$2"; shift 2 ;;
    --workspace) tag="$2"; shift 2 ;;
    --) shift; break ;;
    -*) echo "workspace-attention: unknown option: $1" >&2; exit 2 ;;
    *) dir="$1"; shift ;;
  esac
done

state_dir="${XDG_RUNTIME_DIR:?XDG_RUNTIME_DIR is not set}/workspace-attention"
mkdir -p "$state_dir"

if [ -n "$tag" ]; then
  ident="tag=$tag"
else
  cwd="$(realpath -- "${dir:-$PWD}")"
  ident="cwd=$cwd"
fi

key="$(printf '%s' "$ident" | sha1sum | cut -d' ' -f1)"
pending="$state_dir/$key"
seen="$state_dir/$key.seen"

# Create only if this attention episode is not already recorded (pending or
# already-notified). Atomic write via temp + rename.
if [ ! -e "$pending" ] && [ ! -e "$seen" ]; then
  tmp="$(mktemp "$state_dir/.tmp.XXXXXX")"
  printf '%s\nsource=%s\nmessage=%s\n' "$ident" "$source" "$message" > "$tmp"
  mv -f "$tmp" "$pending"
fi

# Wake xmonad to re-render now. Harmless no-op if xmonad is absent/restarting.
xmonadctl agent-attention-refresh >/dev/null 2>&1 || true
```

- [ ] **Step 4: Run the test to verify it passes**

Run: `nix run nixpkgs#bats -- modules/home-manager/profiles/desktop/xmonad/workspace-attention.bats`
Expected: PASS — all 5 tests pass.

- [ ] **Step 5: Lint the shell (matches the build-time shellcheck)**

Run: `nix run nixpkgs#shellcheck -- modules/home-manager/profiles/desktop/xmonad/workspace-attention.sh`
Expected: no output (clean). Fix any findings, re-run Step 4.

- [ ] **Step 6: Commit** (confirm with owner first)

```bash
git add modules/home-manager/profiles/desktop/xmonad/workspace-attention.sh \
        modules/home-manager/profiles/desktop/xmonad/workspace-attention.bats
git commit -m "workspace-attention: add generic CLI to flag a workspace as needing attention"
```

---

## Task 2: Package the CLI and install it

**Files:**
- Create: `modules/home-manager/profiles/desktop/workspace-attention.nix`
- Modify: `modules/home-manager/profiles/desktop/default.nix` (imports list)

- [ ] **Step 1: Create the package module**

Create `modules/home-manager/profiles/desktop/workspace-attention.nix`:

```nix
{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.profiles.desktop;
  workspace-attention = pkgs.writeShellApplication {
    name = "workspace-attention";
    runtimeInputs = [
      pkgs.coreutils # sha1sum, realpath, mktemp, mv, cut, mkdir
      pkgs.xmonadctl # the "agent-attention-refresh" poke
    ];
    text = builtins.readFile ./xmonad/workspace-attention.sh;
  };
in
{
  options.profiles.desktop.workspaceAttention.package = lib.mkOption {
    type = lib.types.package;
    readOnly = true;
    default = workspace-attention;
    description = "CLI that flags a workspace as needing attention (consumed by xmonad.hs). Referenced by the Claude Code Stop hook.";
  };

  config = lib.mkIf cfg.enable {
    home.packages = [
      workspace-attention
      pkgs.xmonadctl
    ];
  };
}
```

- [ ] **Step 2: Import it from the desktop profile**

In `modules/home-manager/profiles/desktop/default.nix`, add `./workspace-attention.nix` to the `imports` list:

```nix
  imports = [
    inputs.peon-ping.homeManagerModules.default
    ./firefox.nix
    ./polybar.nix
    ./rofi.nix
    ./workspace-attention.nix
    ./xmonad.nix
  ];
```

- [ ] **Step 3: Verify the package builds and the option resolves**

Run: `nix build --no-link --print-out-paths ".#nixosConfigurations.$(hostname).config.home-manager.users.matt.profiles.desktop.workspaceAttention.package"`
Expected: a `/nix/store/...-workspace-attention` path prints (build succeeds; shellcheck passes inside `writeShellApplication`).

If your host's home-manager attribute path differs, instead run `nix flake check` (Step 4) which evaluates everything.

- [ ] **Step 4: Verify the flake still evaluates**

Run: `nix flake check`
Expected: completes without error.

- [ ] **Step 5: Commit** (confirm with owner first)

```bash
git add modules/home-manager/profiles/desktop/workspace-attention.nix \
        modules/home-manager/profiles/desktop/default.nix
git commit -m "workspace-attention: package the CLI and install it on desktop hosts"
```

---

## Task 3: xmonad — attention model, logHook integration, contrast fix

**Files:**
- Modify: `modules/home-manager/profiles/desktop/xmonad/xmonad.hs`

This task adds the consumer side. After it, the bell appears/clears and the popup fires; the refresh command and `M-S-u` come in Task 4.

- [ ] **Step 1: Add imports**

Replace the import for `Data.List` (currently `import Data.List (find, isPrefixOf, sort)`):

```haskell
import Data.List (find, isPrefixOf, isSuffixOf, nub, sort, sortOn)
```

Replace the import for `System.Directory` (currently lists `doesDirectoryExist, doesPathExist, getHomeDirectory, listDirectory`):

```haskell
import System.Directory (doesDirectoryExist, doesPathExist, getHomeDirectory, listDirectory, removeFile, renameFile)
```

Replace the `UrgencyHook` import (currently `import XMonad.Hooks.UrgencyHook (UrgencyHook (..), focusUrgent, readUrgents, withUrgencyHook)`) — `focusUrgent` is no longer used:

```haskell
import XMonad.Hooks.UrgencyHook (UrgencyHook (..), readUrgents, withUrgencyHook)
```

Add these new imports (place near the other `XMonad.Hooks.*` and `System.*` imports):

```haskell
import XMonad.Hooks.ServerMode (serverModeEventHookCmd')
import System.Environment (lookupEnv)
import System.IO (readFile')
```

- [ ] **Step 2: Add `nsRoot` to the `Namespace` record so attribution can resolve tag→path without a filesystem walk**

Replace the `Namespace` data declaration:

```haskell
data Namespace = Namespace
  { nsPrefix :: String,
    -- | Absolute root directory for this namespace (e.g. ~/projects). Used by
    -- attribution to map an existing tag back to its path by string only.
    nsRoot :: FilePath,
    -- | Resolve a key (no prefix) to an absolute path. Nothing if the path
    -- does not exist on disk.
    nsResolve :: String -> IO (Maybe FilePath),
    -- | All picker candidates for this namespace (keys only, no prefix).
    nsCandidates :: IO [String]
  }
```

In `mkPathNamespace`, set the new field — replace the record construction:

```haskell
mkPathNamespace prefix root depth markers =
  Namespace
    { nsPrefix = prefix,
      nsRoot = root,
      nsResolve = resolve,
      nsCandidates = enumerate
    }
```

- [ ] **Step 3: Add the attention model and helpers**

Add this block near the urgency code (e.g. just after the `mkPolybarLogHook` definition, before `setupLogOutput`):

```haskell
-- Agent attention: a cross-process channel under $XDG_RUNTIME_DIR. The
-- `workspace-attention` CLI writes one file per attention episode; xmonad reads
-- them here, attributes each to an existing workspace, renders off-screen ones
-- as urgent, fires a dunst popup once per episode, and clears on view. Keep the
-- subdir name in sync with workspace-attention.sh.
attentionSubdir :: FilePath
attentionSubdir = "workspace-attention"

data Attn = Attn
  { attnFile :: FilePath, -- absolute path to the state file (may end ".seen")
    attnIdent :: Either FilePath String, -- Left cwd | Right explicit tag
    attnSource :: String,
    attnMessage :: String,
    attnNotified :: Bool -- True iff the popup already fired (".seen" suffix)
  }

attentionDir :: IO (Maybe FilePath)
attentionDir = fmap (</> attentionSubdir) <$> lookupEnv "XDG_RUNTIME_DIR"

-- Read and parse all state files. Robust to a missing dir / unreadable files.
readAttn :: IO [Attn]
readAttn = do
  mdir <- attentionDir
  case mdir of
    Nothing -> pure []
    Just dir -> do
      ok <- safeIO False (doesDirectoryExist dir)
      if not ok
        then pure []
        else do
          names <- safeIO [] (listDirectory dir)
          catMaybes <$> traverse (parseAttn dir) names

parseAttn :: FilePath -> FilePath -> IO (Maybe Attn)
parseAttn dir name = do
  let path = dir </> name
      notified = ".seen" `isSuffixOf` name
  -- Ignore the CLI's in-flight temp files.
  if ".tmp." `isPrefixOf` name
    then pure Nothing
    else do
      contents <- safeIO "" (readFile' path)
      let kv =
            mapMaybe
              ( \l -> case break (== '=') l of
                  (k, '=' : v) -> Just (k, v)
                  _ -> Nothing
              )
              (lines contents)
          ident = case (lookup "tag" kv, lookup "cwd" kv) of
            (Just t, _) -> Just (Right t)
            (_, Just c) -> Just (Left c)
            _ -> Nothing
      pure $
        ( \i ->
            Attn
              path
              i
              (maybe "agent" id (lookup "source" kv))
              (maybe "needs attention" id (lookup "message" kv))
              notified
        )
          <$> ident

-- True iff `anc` is `path` or a path-component ancestor of it.
isAncestorOf :: FilePath -> FilePath -> Bool
isAncestorOf anc path = path == anc || (anc ++ "/") `isPrefixOf` path

-- The longest existing tag whose resolved directory is an ancestor of cwd.
attributeCwd :: [(String, FilePath)] -> FilePath -> Maybe String
attributeCwd tagPathList cwd =
  case sortOn (negate . length . snd) [tp | tp@(_, p) <- tagPathList, p `isAncestorOf` cwd] of
    ((t, _) : _) -> Just t
    [] -> Nothing

-- Map each path-namespaced existing tag to its resolved directory (string only).
tagPaths :: [String] -> IO [(String, FilePath)]
tagPaths tags = do
  nss <- namespaces
  pure
    [ (tag, nsRoot ns </> key)
      | tag <- tags,
        Just (pfx, key) <- [splitWorkspace tag],
        Just ns <- [findNamespace pfx nss]
    ]

-- The workspace tag an entry belongs to, or Nothing if it cannot be placed.
attnTag :: [String] -> [(String, FilePath)] -> Attn -> Maybe String
attnTag existing paths e = case attnIdent e of
  Right t -> if t `elem` existing then Just t else Nothing
  Left cwd -> attributeCwd paths cwd

safeRemoveFile :: FilePath -> IO ()
safeRemoveFile p = safeIO () (removeFile p)

markSeen :: FilePath -> IO ()
markSeen p = safeIO () (renameFile p (p ++ ".seen"))

-- Side-effecting per render: clear markers for visible workspaces, fire a popup
-- for each newly-attributed off-screen one, and return the off-screen tags that
-- should show the bell.
processAttention :: [String] -> [String] -> X [String]
processAttention existing visibleTags = do
  paths <- io (tagPaths existing)
  entries <- io readAttn
  fmap catMaybes . forM entries $ \e ->
    case attnTag existing paths e of
      Nothing -> pure Nothing
      Just t
        | t `elem` visibleTags ->
            io (safeRemoveFile (attnFile e)) >> pure Nothing
        | otherwise -> do
            when (not (attnNotified e)) $ do
              safeSpawn notifySend ["-a", attnSource e, "-u", "normal", t, attnMessage e]
              io (markSeen (attnFile e))
            pure (Just t)
```

- [ ] **Step 4: Wire it into `mkPolybarLogHook` (compute attention once, union into the urgent block, unify on visible workspaces, fix the contrast)**

Replace the body of `mkPolybarLogHook` down to the `dynamicLogWithPP` call. Current code:

```haskell
mkPolybarLogHook output = do
  windowCount <- withWindowSet (pure . length . W.index)
  minimizedCount <- withMinimized (pure . length)
  currentTag <- gets (W.currentTag . windowset)
  let urgentExtras = do
        urgents <- readUrgents
        ws <- gets windowset
        let tags =
              Set.toList . Set.fromList . filter validTag $
                mapMaybe (`W.findTag` ws) urgents
            validTag t = t /= currentTag && t /= scratchpadWorkspaceTag
        pure $ case tags of
          [] -> Nothing
          _ -> Just $ highlight colorBase0A (bellIcon ++ " " ++ unwords tags)
```

New:

```haskell
mkPolybarLogHook output = do
  windowCount <- withWindowSet (pure . length . W.index)
  minimizedCount <- withMinimized (pure . length)
  currentTag <- gets (W.currentTag . windowset)
  visibleOther <- gets (map (W.tag . W.workspace) . W.visible . windowset)
  existing <- existingTags
  let visibleTags = currentTag : visibleOther
  agentTags <- processAttention existing visibleTags
  let urgentExtras = do
        urgents <- readUrgents
        ws <- gets windowset
        let winTags = mapMaybe (`W.findTag` ws) urgents
            tags =
              Set.toList . Set.fromList $
                filter validTag (winTags ++ agentTags)
            validTag t = t `notElem` visibleTags && t /= scratchpadWorkspaceTag
        pure $ case tags of
          [] -> Nothing
          _ -> Just $ highlight colorBase0A (bellIcon ++ " " ++ unwords tags)
```

(The rest of `mkPolybarLogHook` — the `dynamicLogWithPP $ filterOutWsPP ...` block and the `where` clause — stays as is.)

- [ ] **Step 5: Fix the urgent-block contrast (pre-existing readability bug, reused by the bells)**

In the `where` clause of `mkPolybarLogHook`, change the highlight foreground from `colorBase01` (a near-background shade) to `colorBase00` (the darkest), which reads cleanly on the `base0A` yellow. Current line:

```haskell
    highlight bg = wrap ("%{B" ++ bg ++ "}%{F" ++ colorBase01 ++ "} ") " %{F-}%{B-}"
```

New:

```haskell
    -- Dark text on the yellow (base0A) background; base01 was too low-contrast.
    -- NOTE: assumes a dark theme (base00 = darkest). On a light theme use a dark
    -- foreground such as base07 instead; verify live in Task 6.
    highlight bg = wrap ("%{B" ++ bg ++ "}%{F" ++ colorBase00 ++ "} ") " %{F-}%{B-}"
```

- [ ] **Step 6: Verify it compiles**

Run: `nixos-rebuild build --flake ".#$(hostname)"`
Expected: build succeeds. (GHC may warn about the not-yet-used `focusAttention`-related helpers only if you reordered — `processAttention`/`tagPaths`/`attnTag` are used by Step 4, so no unused-binding warnings are expected here.)

- [ ] **Step 7: Verify the pure helpers behave (dev-shell ghci spot-check)**

Run:

```bash
nix develop modules/home-manager/profiles/desktop/xmonad -c bash -lc '
  cd modules/home-manager/profiles/desktop/xmonad
  ghci -v0 xmonad.hs <<EOF
isAncestorOf "/home/m/projects/foo" "/home/m/projects/foo/bar"
attributeCwd [("p:foo","/home/m/projects/foo"),("p:foo/bar","/home/m/projects/foo/bar")] "/home/m/projects/foo/bar/x"
attributeCwd [("p:foo","/home/m/projects/foo")] "/home/m/other"
EOF'
```

Expected output:

```
True
Just "p:foo/bar"
Nothing
```

(The `@colorBase00@`-style placeholders in `xmonad.hs` are inside string literals, so the unsubstituted source still loads in ghci.)

- [ ] **Step 8: Commit** (confirm with owner first)

```bash
git add modules/home-manager/profiles/desktop/xmonad/xmonad.hs
git commit -m "xmonad: render and notify agent workspace-attention in the urgent block"
```

---

## Task 4: xmonad — ServerMode refresh command and generalized `M-S-u`

**Files:**
- Modify: `modules/home-manager/profiles/desktop/xmonad/xmonad.hs`

- [ ] **Step 1: Register the refresh command in `handleEventHook`**

Replace `myHandleEventHook`. Current:

```haskell
myHandleEventHook =
  fixSteamFlicker
    <+> onXPropertyChange "WM_NAME" manageZoomHook
    <+> handleEventHook def
```

New:

```haskell
myHandleEventHook =
  fixSteamFlicker
    <+> onXPropertyChange "WM_NAME" manageZoomHook
    -- External wake from the workspace-attention CLI: `xmonadctl
    -- agent-attention-refresh` re-runs the logHook so bells appear promptly.
    -- Fixed one-command list => no arbitrary-exec exposure to X clients.
    <+> serverModeEventHookCmd' (pure [("agent-attention-refresh", refresh)])
    <+> handleEventHook def
```

- [ ] **Step 2: Generalize the `M-S-u` binding**

Change the keybinding (currently `("M-S-u", focusUrgent),`):

```haskell
                          ("M-S-u", focusAttention),
```

- [ ] **Step 3: Define `focusAttention`**

Add near the other action helpers (e.g. after `historyToggle`):

```haskell
-- Jump to the next thing needing attention: window-urgent windows (focus the
-- window) or agent-attention workspaces (view the workspace). Off-screen targets
-- only; window-urgent first. Acting on a target makes it visible, so it drops
-- out next press — repeated M-S-u cycles through everything. Agent jumps are
-- workspace-level by necessity (no agent window handle exists; see the design).
focusAttention :: X ()
focusAttention = do
  ws <- gets windowset
  let curr = W.currentTag ws
      visibleTags = curr : map (W.tag . W.workspace) (W.visible ws)
  urgents <- readUrgents
  existing <- existingTags
  paths <- io (tagPaths existing)
  entries <- io readAttn
  let urgentByTag = mapMaybe (\w -> (\t -> (t, w)) <$> W.findTag w ws) urgents
      agentTags = mapMaybe (attnTag existing paths) entries
      targets =
        filter (`notElem` visibleTags) . nub $
          map fst urgentByTag ++ agentTags
  case targets of
    [] -> pure ()
    (t : _) -> case lookup t urgentByTag of
      Just w -> windows (W.focusWindow w)
      Nothing -> windows (W.greedyView t)
```

- [ ] **Step 4: Verify it compiles**

Run: `nixos-rebuild build --flake ".#$(hostname)"`
Expected: build succeeds, no warnings about unused `focusUrgent` (it was removed from imports in Task 3) or unused helpers.

- [ ] **Step 5: Commit** (confirm with owner first)

```bash
git add modules/home-manager/profiles/desktop/xmonad/xmonad.hs
git commit -m "xmonad: serve agent-attention refresh and generalize M-S-u to it"
```

---

## Task 5: Claude Code `Stop` hook glue + tests

**Files:**
- Create: `modules/home-manager/profiles/base/harnesses/claude-code/claude-stop-attention.sh`
- Test: `modules/home-manager/profiles/base/harnesses/claude-code/claude-stop-attention.bats`
- Modify: `modules/home-manager/profiles/base/harnesses/claude-code/default.nix`

- [ ] **Step 1: Write the failing test**

Create `modules/home-manager/profiles/base/harnesses/claude-code/claude-stop-attention.bats`:

```bash
#!/usr/bin/env bats
# Run: nix run nixpkgs#bats -- modules/home-manager/profiles/base/harnesses/claude-code/claude-stop-attention.bats

setup() {
  STUBBIN="$BATS_TEST_TMPDIR/bin"
  mkdir -p "$STUBBIN"
  cat > "$STUBBIN/workspace-attention" <<'EOF'
#!/usr/bin/env bash
echo "$@" > "$RECORD"
EOF
  chmod +x "$STUBBIN/workspace-attention"
  export PATH="$STUBBIN:$PATH"
  export RECORD="$BATS_TEST_TMPDIR/record"
  SCRIPT="${BATS_TEST_DIRNAME}/claude-stop-attention.sh"
}

@test "extracts .cwd from stdin JSON and calls the CLI" {
  echo '{"cwd":"/home/x/projects/foo","hook_event_name":"Stop"}' | bash "$SCRIPT"
  run cat "$RECORD"
  [[ "$output" == *"--source claude"* ]]
  [[ "$output" == *"/home/x/projects/foo"* ]]
}

@test "no .cwd -> no call" {
  echo '{"hook_event_name":"Stop"}' | bash "$SCRIPT"
  [ ! -f "$RECORD" ]
}
```

- [ ] **Step 2: Run the test to verify it fails**

Run: `nix run nixpkgs#bats -- modules/home-manager/profiles/base/harnesses/claude-code/claude-stop-attention.bats`
Expected: FAIL — `claude-stop-attention.sh` does not exist.

- [ ] **Step 3: Write the glue script**

Create `modules/home-manager/profiles/base/harnesses/claude-code/claude-stop-attention.sh`:

```bash
# Claude Code Stop hook: when the top-level agent finishes a turn, flag the
# workspace that owns the session's cwd. Reads the hook payload JSON on stdin.
set -euo pipefail

cwd="$(jq -r '.cwd // empty')"
if [ -n "$cwd" ]; then
  exec workspace-attention --source claude --message "agent finished" "$cwd"
fi
```

- [ ] **Step 4: Run the test to verify it passes**

Run: `nix run nixpkgs#bats -- modules/home-manager/profiles/base/harnesses/claude-code/claude-stop-attention.bats`
Expected: PASS — both tests pass.

- [ ] **Step 5: Lint the shell**

Run: `nix run nixpkgs#shellcheck -- modules/home-manager/profiles/base/harnesses/claude-code/claude-stop-attention.sh`
Expected: clean.

- [ ] **Step 6: Wire the hook into Claude Code settings (gated on desktop)**

In `modules/home-manager/profiles/base/harnesses/claude-code/default.nix`:

(a) Add a `let` binding for the wrapped glue app, after the `statuslineScript` binding (inside the top `let ... in`):

```nix
  attentionStopApp = pkgs.writeShellApplication {
    name = "claude-stop-attention";
    runtimeInputs = [
      pkgs.jq
      config.profiles.desktop.workspaceAttention.package
    ];
    text = builtins.readFile ./claude-stop-attention.sh;
  };
```

(b) Replace the `hooks` attribute (currently `hooks = lib.mkIf config.programs.peon-ping.enable { Notification = [ ... ]; };`) with one that merges the peon and attention hooks under their own conditions:

```nix
        hooks =
          (lib.optionalAttrs config.programs.peon-ping.enable {
            Notification = [
              {
                hooks = [
                  {
                    type = "command";
                    command = "${config.programs.peon-ping.package}/bin/peon";
                  }
                ];
              }
            ];
          })
          // (lib.optionalAttrs config.profiles.desktop.enable {
            Stop = [
              {
                hooks = [
                  {
                    type = "command";
                    command = "${attentionStopApp}/bin/claude-stop-attention";
                  }
                ];
              }
            ];
          });
```

- [ ] **Step 7: Verify the flake evaluates and the settings render the Stop hook**

Run: `nix flake check`
Expected: completes without error.

Run: `nix build --no-link --print-out-paths ".#nixosConfigurations.$(hostname).config.home-manager.users.matt.programs.claude-code.settings" 2>/dev/null || nixos-rebuild build --flake ".#$(hostname)"`
Expected: builds. (Optional sanity check after switching in Task 6: `jq '.hooks.Stop' ~/.claude/settings.json` shows the `claude-stop-attention` command.)

- [ ] **Step 8: Commit** (confirm with owner first)

```bash
git add modules/home-manager/profiles/base/harnesses/claude-code/claude-stop-attention.sh \
        modules/home-manager/profiles/base/harnesses/claude-code/claude-stop-attention.bats \
        modules/home-manager/profiles/base/harnesses/claude-code/default.nix
git commit -m "claude-code: flag the workspace on Stop via workspace-attention (desktop hosts)"
```

---

## Task 6: Whole-system build, live smoke test, formatting

**Files:** none (verification only), plus any `nixfmt` touch-ups.

- [ ] **Step 1: Format all changed Nix files**

Run: `nix run nixpkgs#nixfmt-rfc-style -- modules/home-manager/profiles/desktop/workspace-attention.nix modules/home-manager/profiles/desktop/default.nix modules/home-manager/profiles/base/harnesses/claude-code/default.nix`
Expected: files reformatted in place (or already formatted). The pre-commit hook enforces this.

- [ ] **Step 2: Full evaluation + build**

Run: `nix flake check && nixos-rebuild build --flake ".#$(hostname)"`
Expected: both succeed.

- [ ] **Step 3: Apply and reload xmonad**

Run: `sudo nixos-rebuild switch --flake ".#$(hostname)"`
Then reload xmonad to pick up the new config: press `Super+q` (xmonad recompile/restart).
Expected: switch succeeds; xmonad restarts without falling back (no error popup).

- [ ] **Step 4: Live smoke matrix**

Confirm each (these map to the spec's edge-case table):

  - [ ] **Off-screen agent → bell + popup.** In a `p:`-named workspace whose cwd is a project under `~/projects`, run `workspace-attention --source claude --message "agent finished"` from that project dir, then immediately switch to another workspace. Expect: a 🔔 yellow block in polybar with the workspace name (now **readable** — dark text), and a dunst popup titled with the tag, body "agent finished".
  - [ ] **`M-S-u` jumps.** Press `Super+Shift+u`; expect to land on that workspace and the bell to clear.
  - [ ] **Watching = silent.** From a project dir, switch to that same workspace first, then run `workspace-attention` for it (e.g. via a second terminal `cd`'d there). Expect: no bell, no popup (cleared on view).
  - [ ] **One popup per episode.** With the workspace off-screen, run `workspace-attention` twice for the same dir. Expect: exactly one popup, one bell.
  - [ ] **Two workspaces.** Trigger attention in two different off-screen project workspaces. Expect: both names in the bell block; `Super+Shift+u` steps through them.
  - [ ] **`--workspace` escape hatch.** With a non-project workspace open and off-screen named e.g. `scratch`, run `workspace-attention --workspace scratch`. Expect: bell on `scratch`.
  - [ ] **Real agent end-to-end.** Start a Claude Code session in a project workspace, switch away, let it finish a turn. Expect: bell + popup for that workspace; `Super+Shift+u` jumps there.
  - [ ] **Logout/reboot leaves no stale bells** (sanity: `ls "$XDG_RUNTIME_DIR/workspace-attention"` is gone after a fresh login).

If any check fails, debug with: `ls -la "$XDG_RUNTIME_DIR/workspace-attention"` (state files), `dunstctl history` (popups), and the xmonad log/`Super+q` for compile errors.

- [ ] **Step 5: Final review + (optional) squash/cleanup commit** (confirm with owner first)

If `nixfmt` changed files after the per-task commits, commit the formatting:

```bash
git add -A
git commit -m "workspace-attention: nixfmt"
```

---

## Self-Review (performed against the spec)

**Spec coverage:**
- Trigger = `Stop`, top-level → Task 5 glue (hooks `Stop`; `SubagentStop` not hooked). ✓
- Generic `workspace-attention` CLI + `--source`/`--message`/`--workspace`/`DIR` → Task 1. ✓
- State in `$XDG_RUNTIME_DIR/workspace-attention/`, `sha1` keys, `<key>`/`<key>.seen`, atomic write, create-if-absent → Task 1 + tests. ✓
- xmonad attribution (longest existing tag whose path is a cwd ancestor; explicit tag if existing) → Task 3 (`attributeCwd`/`attnTag`/`tagPaths`). ✓
- Clear-on-view (visible incl. other screens), popup by xmonad for non-visible & not-`.seen`, union into existing `base0A` bell, unify window-urgency on `visibleTags` → Task 3 Step 4. ✓
- `serverModeEventHookCmd'` fixed command + `pkgs.xmonadctl` → Task 2 (package) + Task 4 (command). ✓
- Generalized `M-S-u` (window-level for urgent windows, workspace-level for agents) → Task 4 `focusAttention`. ✓
- Gating on `profiles.desktop.enable`; peon untouched → Task 5 Step 6. ✓
- Contrast follow-up folded in → Task 3 Step 5. ✓
- Non-goals (remote agents, window-level agent focus, idle timer, click-to-jump) → not implemented, as intended. ✓

**Placeholder scan:** no `TBD`/`TODO`/"add error handling"/"similar to" — every code and command step is concrete. ✓

**Type/name consistency:** `attentionSubdir`, `Attn`{`attnFile`,`attnIdent`,`attnSource`,`attnMessage`,`attnNotified`}, `attentionDir`, `readAttn`, `parseAttn`, `isAncestorOf`, `attributeCwd`, `tagPaths`, `attnTag`, `safeRemoveFile`, `markSeen`, `processAttention`, `focusAttention`, `nsRoot`, command `agent-attention-refresh`, state subdir `workspace-attention` — all defined once and referenced consistently across Tasks 1/3/4/5. ✓
