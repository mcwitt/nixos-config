# Design: X11/xmonad → Wayland/ewm migration

Date: 2026-06-27
Branch: `satori-wayland-migration`
Status: approved (brainstorming), pending implementation plan

## 1. Goal & scope

Replace the entire X11/xmonad desktop stack with [ewm](https://codeberg.org/ezemtsov/ewm/)
(Emacs Wayland Manager) across all three desktop hosts — **satori, golem, karakuri**.

End state:

- No `services.xserver`, no `displayManager.lightdm`, no `desktopManager.xterm` on any host.
- No xmonad, polybar, picom, rofi. The `xmonad/` Haskell sources are deleted.
- A Wayland session driven by ewm, started by greetd/tuigreet.

There is **no live fallback session**. Rollback is via `git revert` + redeploy or by
selecting the previous boot generation. (A staged parallel profile keeps the branch
buildable during the migration; see §3.)

## 2. Core architecture — the inversion

ewm is a Rust/[Smithay](https://github.com/Smithay/smithay) Wayland compositor loaded as a
**dynamic module inside one `emacs-pgtk` daemon**, launched from a TTY. **Emacs becomes the
window manager.** Wayland applications render as Emacs **buffers**; window management is
Emacs window/buffer management:

- Tile with Emacs windows (`C-x 2` / `C-x 3`).
- Switch anything with `consult-buffer` (`s-d` → `ewm-launch-app`/`consult-buffer`).
- Focus-jump with `ace-window` / `avy` (the EasyMotion analog).
- Group work with `project.el` + `tab-bar` (project-named workspaces over `~/projects`).

The compositor is launched by the ewm NixOS module as:

```
emacs --fg-daemon --eval "(require 'ewm)" --eval "(ewm-start-module)"
```

### The frames-only-mode inversion

Today the Emacs config enables `frames-only-mode`
(`modules/home-manager/profiles/base/emacs/default.nix`), so every Emacs buffer spawns its
own X frame that xmonad tiles. The effective model is "one Emacs buffer = one X frame = one
xmonad tile."

ewm reverses this: Emacs *is* the compositor, external apps become buffers, and you tile
with Emacs-internal windows. Therefore **`frames-only-mode` is disabled on Linux** under the
Wayland profile, and Emacs-internal windows become the tiling primitive. This matches the
chosen "embrace Emacs-native idioms" direction.

### One unified Emacs

The compositor-hosting Emacs **is** the full daily-driver Emacs config (all packages),
switched to the pgtk build on Linux. There is no separate "thin WM Emacs." Consequence: a
broken Emacs init means no graphical desktop; recovery is via TTY/SSH or selecting the
previous boot generation.

## 3. Repository structuring & rollout strategy

The end state is a full rip-out, but each host must be independently verifiable (satori
first, per the branch name). To get both, the migration is staged through a parallel
profile:

1. Add a **new parallel profile** `profiles.wayland` on both the NixOS
   (`modules/nixos/profiles/`) and home-manager (`modules/home-manager/profiles/`) sides,
   holding the ewm stack. Leave the existing `profiles.desktop` (xmonad) untouched.
2. Migrate **satori** from `profiles.desktop` to `profiles.wayland`. Deploy and verify on
   real hardware (NVIDIA is the key risk — see §8).
3. Migrate **golem**, then **karakuri**.
4. Once all three hosts are verified on ewm, **delete `profiles.desktop`** and the
   xmonad/polybar/picom/rofi modules plus the `xmonad/` sources, then **rename
   `profiles.wayland` → `profiles.desktop`**.

Net result is the full rip-out. The parallel profile is scaffolding so the branch stays
buildable and bisectable and so a single host can be reverted with a one-line profile flip
during the migration window.

Rejected alternative: big-bang rewrite of `profiles.desktop` in place. Rejected because the
desktop profile is shared by all three hosts, so an in-place rewrite changes all three at
once with no incremental verification.

## 4. NixOS layer changes

### flake.nix

- Add input:
  ```nix
  ewm.url = "https://codeberg.org/ezemtsov/ewm/archive/master.tar.gz";
  ewm.inputs.nixpkgs.follows = "nixpkgs"; # ewm's flake takes a nixpkgs input
  ```
  Pin via `flake.lock`; ewm tracks `master`, so expect churn.
- Apply `ewm.overlays.default` (adds the `ewm` package) on the desktop hosts.
- Import `ewm.nixosModules.default` for satori/golem/karakuri (via `makeNixosSystem`).

### profiles.wayland (NixOS side)

- `programs.ewm.enable = true;`
- `programs.ewm.emacsPackage` = the host's home-manager Emacs `finalPackage` (a pgtk build
  that includes the ewm elisp library + `vterm`), so the compositor Emacs *is* the
  configured daily-driver Emacs. This requires referencing
  `config.home-manager.users.<user>.programs.emacs.finalPackage` from the NixOS module.
- `programs.ewm.screencast.enable = true;` (PipeWire screencast / portals).
- `programs.ewm.renderDevice` set per host where GPU selection matters (see §6).

### Login

- `services.greetd` with **tuigreet** on TTY1; default session = the ewm wayland-session
  registered by the module (`services.displayManager.sessionPackages`).

### Removed

- `services.xserver.*` (including `xkb`, `xrandrHeads`).
- `services.displayManager.lightdm` and the GTK greeter.
- `services.xserver.desktopManager.xterm`.
- `services.autorandr` (karakuri).

### Kept / provided

- PipeWire (unchanged), `programs.dconf`, fonts.
- The ewm module already provides XDG portals, gnome-keyring, WirePlumber, and
  `NIXOS_OZONE_WL=1`.

## 5. Home-manager layer changes (profiles.wayland)

### Emacs

- `programs.emacs.package = emacs-pgtk` on Linux; keep `emacs-macport` on Darwin.
- Add the ewm elisp package (`config.programs.ewm.ewmPackage` equivalent / overlay) and
  `vterm` to the Emacs package set.
- **Disable `frames-only-mode` on Linux** under this profile.
- New `ewm.nix` Emacs module:
  - `ewm-output-config` per host (see §6).
  - `ewm-mode-map` bindings: `s-d` launcher (`ewm-launch-app` / `consult-buffer`), media
    keys (brightness via `brightnessctl`, volume/mute via `wpctl`).
  - Workspaces via `project.el` + `tab-bar` (project-named over `~/projects`).
  - `display-buffer` rules to replace the old named scratchpads (org-capture, audio
    control, btop) with curated buffer-display behavior.

### Desktop shell / peripherals

| Concern        | X11 today                       | Wayland/ewm replacement                          |
|----------------|---------------------------------|--------------------------------------------------|
| Compositor/WM  | xmonad + picom                  | ewm (Emacs)                                       |
| Bar + tray     | polybar                         | **waybar** (layer-shell) + SNI tray               |
| Launcher       | rofi                            | `consult-buffer` / `ewm-launch-app` (`s-d`)       |
| Notifications  | dunst                           | **mako** (stylix has a mako target)               |
| Lock / idle    | `dm-tool switch-to-greeter`     | **swaylock + swayidle**                           |
| Wallpaper      | feh                             | ewm-native wallpaper; fallback **swaybg**         |
| Screenshots    | flameshot                       | **grim + slurp + satty**                          |
| Clipboard      | xclip                           | **wl-clipboard** (required by ewm)                |
| Brightness     | xbacklight                      | brightnessctl                                     |
| Night light    | gammastep (randr)               | gammastep (wlr-gamma) — risk, see §8.2            |

- **waybar** ports the polybar modules cpu / mem / temperature / network / battery /
  pulseaudio(volume) / clock and provides the **SNI system tray**. Workspaces and window
  title come from Emacs (tab-bar / modeline), not waybar.

## 6. Per-host specifics

- **satori**: outputs `DP-2` (primary) + `DP-0` → `ewm-output-config`. Single RTX 3090 →
  set `programs.ewm.renderDevice` to its `renderDxxx` node; ensure `nvidia-drm.modeset=1`.
- **golem**: outputs `DP-4` (primary) + `DP-2` **rotated left** → output transform in
  `ewm-output-config`. NVIDIA handling like satori.
- **karakuri**: laptop + dock hotplug (replaces autorandr) → `ewm-output-config` with
  hotplug handling; HiDPI per-output `scale` (previously DPI 224). Intel/AMD GPU — simpler
  than the NVIDIA hosts.
- Per-host font/scale: the old per-host polybar/rofi DPI (satori 163, golem 183,
  karakuri 224) maps to per-output Wayland `scale` plus waybar/mako font sizes.

## 7. Theming (stylix)

- Swap the X11 stylix targets in `modules/stylix/linux.nix` for Wayland targets (mako,
  swaylock, cursor).
- Keep the base16 scheme and the **base06 desktop accent** convention for waybar/mako focus
  colors (consistent with the existing desktop accent rule).
- Emacs theme is unchanged (base0D remains the Emacs/Modus blue).

## 8. Risks & open questions

1. **NVIDIA + Smithay (satori, golem)** — the biggest risk. Requires `nvidia-drm.modeset=1`,
   a recent driver, and GBM; `programs.ewm.renderDevice` pins the GPU. Verify fully on
   satori before touching golem.
2. **gammastep gamma protocol** — depends on ewm/Smithay implementing `wlr-gamma-control`.
   If unsupported, fall back to ewm-native appearance/night settings or drop gammastep.
   Verify early.
3. **ewm maturity** — young, master-tracked project; pin the input and expect churn. Keep a
   TTY/SSH recovery path. A broken Emacs init means no desktop (recover via previous boot
   generation).
4. **Emacs config under pgtk** — switching the whole config to pgtk is low-risk (pgtk runs
   under X too), but verify org-latex-preview / native-comp / heavy packages still build and
   run.

## 9. Testing & rollback

- Each step: `nix flake check` and `nixos-rebuild build --flake .#<host>` before any switch.
- satori is local (`nixos-rebuild switch --flake .`); golem and karakuri deploy via the
  nixos-config-private input repointing workflow (point that input's flake ref at this
  branch).
- Rollback = previous boot generation or `git revert`. The xmonad profile stays in-tree
  until all three hosts are verified (§3), so reverting one host during the migration window
  is a one-line profile flip.

## 10. Implementation sequencing (preview for the plan)

1. flake input + overlay.
2. `profiles.wayland` NixOS + home-manager scaffolding.
3. ewm Emacs: pgtk build, ewm module load, bindings, drop frames-only-mode.
4. waybar / mako / swaylock+swayidle / wallpaper / screenshots / clipboard / gammastep.
5. satori host cutover + hardware verification (NVIDIA, outputs, gamma).
6. golem cutover.
7. karakuri cutover.
8. Delete the xmonad/polybar/picom/rofi stack and `xmonad/` sources; rename
   `profiles.wayland` → `profiles.desktop`.

---

## Addendum (2026-07-10): decompose profiles into orthogonal components

Approved design change, superseding §3's "rename `profiles.wayland` → `profiles.desktop`"
endgame. `profiles.desktop` is split into orthogonal components and **deleted outright**
(stale enables in nixos-config-private fail loudly at eval instead of silently re-enabling
lightdm next to greetd).

NixOS side: `profiles.audio` (pipewire/rtkit, from `desktop/pipewire.nix`),
`profiles.x11` (xserver/lightdm/xterm/dconf; dir renamed from `desktop/`),
`profiles.wayland` (unchanged; keeps its name permanently — Task 11 no longer renames).

home-manager side: `profiles.gui-apps` (ghostty, chromium, firefox, mpv, zathura,
vscodium, udiskie, thunar, pavucontrol, imv, xdg/mime + org-protocol — the blocks
previously duplicated between `desktop/` and `wayland/`), `profiles.x11` (xmonad,
polybar, rofi, picom, dunst, flameshot, gammastep-randr, urxvt, feh, xclip, xsession),
`profiles.wayland` (session-specific only: emacs-ewm, waybar, mako/swaylock/swayidle/
swaybg/grim/gammastep-wayland).

Hosts compose explicitly: satori/golem/karakuri = `audio + wayland` (nixos),
`gui-apps + wayland` (hm). `x11` is enabled nowhere; it exists as the rollback flip
until Task 11 deletes it. Task 11 simplifies to: delete both `x11` profiles + xmonad
sources.

Private repo: per migrated host set `audio + wayland` (nixos) and `gui-apps + wayland`
(per user, incl. kiso); until then private deploys fail at eval by design.

Verification bar: all six hosts eval; three desktop hosts build; `nix flake check`
green; satori closure spot-check contains pipewire, ghostty, firefox (guards against
gates going dark).
