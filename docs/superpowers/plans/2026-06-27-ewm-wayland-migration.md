# ewm Wayland Migration Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Replace the X11/xmonad desktop stack with ewm (Emacs Wayland Manager) on satori, golem, and karakuri, then delete the old stack.

**Architecture:** ewm is a Rust/Smithay Wayland compositor loaded as a dynamic module inside one `emacs-pgtk` `--fg-daemon` (Emacs *is* the WM; Wayland apps are buffers). A new parallel `profiles.wayland` (NixOS + home-manager) carries the ewm stack; hosts migrate one at a time off `profiles.desktop`; once all three are verified the old profile and xmonad sources are deleted and `profiles.wayland` is renamed to `profiles.desktop`.

**Tech Stack:** Nix flakes, NixOS modules, home-manager, rycee `emacs-init`, ewm (`codeberg.org/ezemtsov/ewm`), `emacs-pgtk`, greetd+tuigreet, waybar, mako, swaylock/swayidle, stylix.

## Global Constraints

- **Do NOT run `git commit` or `git push`.** Standing user rule overrides the plan's commit steps. Each "Checkpoint" stages files (`git add`) and states a proposed message; the user commits. (Ref: `feedback_no_commit_without_asking`.)
- **Do NOT switch/cutover a live session.** The implementer's deliverable for each host is a *successful build* (`nixos-rebuild build`), not `switch`. The user performs the actual login-session cutover and hardware verification (NVIDIA, real outputs, gamma). Steps flag this explicitly.
- **No fallback session is the end state**, but `profiles.desktop` (xmonad) stays in-tree until all three hosts are verified (Task 11 deletes it).
- ewm tracks `master`; pin via `flake.lock` and expect churn.
- All three desktop hosts are single-user **`matt`**; the NixOS→home-manager Emacs reference uses `config.home-manager.users.matt`.
- Verification commands run from the worktree root `/home/matt/projects/nixos-config.satori-wayland-migration`.
- Worktrees lack the generated pre-commit config, so any commit the user makes here needs `--no-verify` (Ref: `project_precommit_and_worktrees`). Run `nix fmt` before staging.
- Keep the `base06` (dark blue) desktop accent convention for focus colors in waybar/mako (Ref: `project_theme_accent_color`).

---

### Task 1: Add ewm flake input + overlay + module wiring

**Files:**
- Modify: `flake.nix` (inputs block; `makeNixosSystem` `modules` + overlays list)

**Interfaces:**
- Produces: flake input `ewm` with `ewm.overlays.default` (adds `pkgs.ewm`), `ewm.nixosModules.default` (defines `programs.ewm.*`). Both available to all `makeNixosSystem` hosts (golem/karakuri/satori).

- [ ] **Step 1: Add the input.** In `flake.nix` `inputs`, after the `emacs-overlay` line, add:

```nix
    ewm.url = "https://codeberg.org/ezemtsov/ewm/archive/master.tar.gz";
    ewm.inputs.nixpkgs.follows = "nixpkgs";
```

- [ ] **Step 2: Apply the overlay + module in `makeNixosSystem`.** The `outputs` function already destructures named inputs and passes `inputs` through; `ewm` is reachable as `inputs.ewm`. In the `makeNixosSystem` `modules` list, add the ewm NixOS module next to `self.nixosModules.default`:

```nix
            self.nixosModules.default
            inputs.ewm.nixosModules.default
```

  And in the same builder's `nixpkgs.overlays` list (the inline module), add `inputs.ewm.overlays.default` after `emacs-overlay.overlay`:

```nix
                    emacs-overlay.overlay
                    inputs.ewm.overlays.default
```

- [ ] **Step 3: Lock the input.**

Run: `nix flake lock`
Expected: `flake.lock` gains an `ewm` node, no errors.

- [ ] **Step 4: Verify the flake still evaluates and ewm options exist.**

Run: `nix flake check 2>&1 | tail -20`
Expected: completes without eval errors (pre-existing hlint/ormolu/nixfmt checks may run; no new errors).

Run: `nix eval --raw '.#nixosConfigurations.satori.options.programs.ewm.enable.type.description' 2>/dev/null || nix eval '.#nixosConfigurations.satori.config.programs.ewm.enable'`
Expected: prints `false` (option present, default off). Confirms the module is wired and inert until enabled.

- [ ] **Step 5: Checkpoint.** Run `nix fmt`, then `git add flake.nix flake.lock`. Proposed message: `flake: add ewm input, overlay, and NixOS module`. (Do not commit — user commits.)

---

### Task 2: Scaffold `profiles.wayland` enable options (NixOS + home-manager)

**Files:**
- Create: `modules/nixos/profiles/wayland/default.nix`
- Create: `modules/home-manager/profiles/wayland/default.nix`
- Modify: `modules/nixos/profiles/default.nix` (add `./wayland` import)
- Modify: `modules/home-manager/profiles/default.nix` (add `./wayland` import)

**Interfaces:**
- Produces: `options.profiles.wayland.enable` on both the NixOS and home-manager option trees. Inert (no config) until later tasks fill in the `config` blocks.

- [ ] **Step 1: NixOS profile skeleton.** Create `modules/nixos/profiles/wayland/default.nix`:

```nix
{ config, lib, ... }:
{
  imports = [ ../desktop/pipewire.nix ];

  options.profiles.wayland.enable =
    lib.mkEnableOption "Profile for machines running the ewm (Wayland) desktop";

  config = lib.mkIf config.profiles.wayland.enable {
    # filled in by later tasks
  };
}
```

- [ ] **Step 2: home-manager profile skeleton.** Create `modules/home-manager/profiles/wayland/default.nix`:

```nix
{ config, lib, ... }:
{
  options.profiles.wayland.enable =
    lib.mkEnableOption "Profile for the ewm (Wayland) graphical desktop";

  config = lib.mkIf config.profiles.wayland.enable {
    # filled in by later tasks
  };
}
```

- [ ] **Step 3: Import both.** In `modules/nixos/profiles/default.nix` add `./wayland` to `imports`. In `modules/home-manager/profiles/default.nix` add `./wayland` to `imports`.

- [ ] **Step 4: Verify eval.**

Run: `nix eval '.#nixosConfigurations.satori.config.profiles.wayland.enable'`
Expected: `false`.

Run: `nixos-rebuild build --flake '.#satori' 2>&1 | tail -5`
Expected: builds successfully (satori still on `profiles.desktop`; the new option is inert).

- [ ] **Step 5: Checkpoint.** `nix fmt`; `git add` the four files. Proposed message: `profiles: scaffold parallel wayland profile (nixos + home-manager)`.

---

### Task 3: NixOS wayland profile — ewm service + greetd login

**Files:**
- Modify: `modules/nixos/profiles/wayland/default.nix`

**Interfaces:**
- Consumes: `programs.ewm.*` (from Task 1), `config.home-manager.users.matt.programs.emacs.finalPackage` (set pgtk + ewm in Task 4).
- Produces: an enabled ewm session registered with greetd; `programs.ewm.renderDevice` left `null` here (set per host in Tasks 8–10).

- [ ] **Step 1: Fill the `config` block.** Replace the placeholder in `modules/nixos/profiles/wayland/default.nix`'s `config` with:

```nix
  config = lib.mkIf config.profiles.wayland.enable {
    # required for `gtk.enable = true` in home-manager
    programs.dconf.enable = true;

    programs.ewm = {
      enable = true;
      # One unified Emacs: the compositor host IS the user's full daily-driver
      # Emacs (pgtk + ewm elisp + vterm), built by home-manager. See Task 4.
      emacsPackage = config.home-manager.users.matt.programs.emacs.finalPackage;
      screencast.enable = true;
    };

    # Minimal TTY greeter; default session is the ewm wayland-session that the
    # ewm module registers via services.displayManager.sessionPackages.
    services.greetd = {
      enable = true;
      settings.default_session = {
        command = "${lib.getExe pkgs.greetd.tuigreet} --time --remember --asterisks --cmd ewm";
        user = "greeter";
      };
    };

    environment.systemPackages = with pkgs; [
      wl-clipboard # ewm copy/paste between Emacs and Wayland apps
      brightnessctl # media-key brightness
    ];
  };
```

  Add `pkgs` to the module's argument set: change the header to `{ config, lib, pkgs, ... }:`.

- [ ] **Step 2: Confirm the registered session name.** The `--cmd ewm` above assumes the module installs `share/wayland-sessions/ewm.desktop` (confirmed: `install -Dm644 .../ewm.desktop .../wayland-sessions/ewm.desktop`). Verify the session name after Task 4 builds (Step in Task 8). If the desktop file's `Name`/exec differs, adjust `--cmd`.

- [ ] **Step 3: Eval guard.** This task cannot fully build until Task 4 makes Emacs pgtk; it only needs to evaluate. Temporarily enable it on satori to type-check:

Run: `nix eval '.#nixosConfigurations.satori.config.services.greetd.enable' 2>&1 | tail -5`
Expected: `false` (satori not yet on wayland; eval of the option tree succeeds with no errors).

- [ ] **Step 4: Checkpoint.** `nix fmt`; `git add modules/nixos/profiles/wayland/default.nix`. Proposed message: `profiles/wayland: enable ewm + greetd login (nixos)`.

---

### Task 4: home-manager wayland profile — pgtk Emacs + ewm Emacs config

**Files:**
- Create: `modules/home-manager/profiles/wayland/emacs-ewm.nix`
- Modify: `modules/home-manager/profiles/wayland/default.nix`
- Modify: `modules/home-manager/profiles/base/emacs/default.nix` (pgtk on Linux; drop frames-only-mode on Linux when wayland profile is on)

**Interfaces:**
- Consumes: `pkgs.ewm` (overlay), the rycee `emacs-init` module (`programs.emacs.*`).
- Produces: `config.programs.emacs.finalPackage` = pgtk Emacs including the ewm elisp library + `vterm`, with `ewm-output-config` settable per host via the option `programs.ewmOutputConfig` (see Step 4).

- [ ] **Step 1: Locate the ewm elisp derivation.** The compositor package `pkgs.ewm` ships the `ewm` Emacs library. Find how it's exposed:

Run: `nix eval --json '.#nixosConfigurations.satori.config.programs.ewm.ewmPackage.outPath' 2>/dev/null; nix build --no-link --print-out-paths '.#nixosConfigurations.satori.config.programs.ewm.ewmPackage' 2>/dev/null | tail -1`
Then inspect: `ls -R "$(nix build --no-link --print-out-paths '.#nixosConfigurations.satori.config.programs.ewm.ewmPackage')" | grep -i 'ewm.el\|site-lisp' | head`

Expected: reveals an Emacs package (an `emacsPackages`-style derivation containing `ewm.el`). The NixOS module's `ewmPackage` default is exactly the elisp package consumed by `emacsWithPackagesFromUsePackage` (per upstream's NixOS example: `extraEmacsPackages = epkgs: [ config.programs.ewm.ewmPackage epkgs.vterm ]`). Capture its attribute path for Step 2. If `ewmPackage` is not an Emacs-loadable package, fall back to `inputs.ewm.packages.<system>.default` and look for a `passthru.emacsPackage` / `share/emacs/site-lisp/ewm`.

- [ ] **Step 2: Switch Emacs to pgtk + add ewm/vterm (Linux only).** In `modules/home-manager/profiles/base/emacs/default.nix`, change the package line (currently `package = if pkgs.stdenv.isDarwin then pkgs.emacs-macport else pkgs.emacs-unstable;`) to:

```nix
      package =
        if pkgs.stdenv.isDarwin then pkgs.emacs-macport else pkgs.emacs-pgtk;
```

  Add the ewm elisp + vterm to the Emacs package set **only when the wayland profile is on**, so non-wayland Linux builds are unaffected. After the `programs.emacs.init` block, add:

```nix
    programs.emacs.extraPackages =
      epkgs:
      lib.optionals (pkgs.stdenv.isLinux && config.profiles.wayland.enable) [
        # ewm elisp library (attr resolved in Step 1) + vterm
        epkgs.vterm
        config.programs.emacs.ewmEmacsPackage # set in emacs-ewm.nix, see Step 4
      ];
```

  (`extraPackages` is provided by the rycee `emacs-init` module and threads through `emacsWithPackagesFromUsePackage`.)

- [ ] **Step 3: Drop frames-only-mode on wayland.** In the same file, the `frames-only-mode` usePackage block sets `enable = true`. Gate it off under wayland by changing its `enable`:

```nix
      frames-only-mode = {
        enable = !(pkgs.stdenv.isLinux && config.profiles.wayland.enable);
        config = lib.mkAfter ''
          (frames-only-mode 1)
        '';
      };
```

  Rationale: under ewm you tile with Emacs-internal windows; frames-only-mode would fight that.

- [ ] **Step 4: ewm Emacs config module.** Create `modules/home-manager/profiles/wayland/emacs-ewm.nix`:

```nix
{
  config,
  lib,
  pkgs,
  ...
}:
{
  # Per-host output configuration, injected as an elisp alist. Hosts set
  # programs.ewmOutputConfig (see Tasks 8-10). Example value:
  #   ''(("DP-2" :width 3840 :height 2160 :scale 1.5 :x 0 :y 0)
  #      ("DP-0" :width 3840 :height 2160 :scale 1.5 :x 2560 :y 0))''
  options.programs.ewmOutputConfig = lib.mkOption {
    type = lib.types.lines;
    default = "nil";
    description = "Elisp value for `ewm-output-config' (a quoted alist or nil).";
  };

  # Surface the resolved ewm elisp package so base/emacs/default.nix can add it
  # to extraPackages (attr from Task 4 Step 1).
  options.programs.emacs.ewmEmacsPackage = lib.mkOption {
    type = lib.types.package;
    description = "Emacs-loadable ewm library package.";
  };

  config = lib.mkIf (config.profiles.wayland.enable && pkgs.stdenv.isLinux) {

    # Resolved in Task 4 Step 1; the NixOS module's ewmPackage is the same elisp
    # derivation. We read it back through osConfig to avoid duplicating the path.
    programs.emacs.ewmEmacsPackage = config.osConfig.programs.ewm.ewmPackage;

    programs.emacs.init.usePackage.ewm = {
      enable = true;
      demand = true;
      config = ''
        (setopt ewm-output-config '${"\${config.programs.ewmOutputConfig}"})
      '';
      extraConfig = ''
        :bind (:map ewm-mode-map
               ;; Launch / switch everything
               ("s-d" . consult-buffer)
               ("s-<return>" . vterm)
               ;; Media keys
               ("<XF86MonBrightnessUp>"   . (lambda () (interactive)
                                              (start-process "bri" nil "brightnessctl" "set" "5%+")))
               ("<XF86MonBrightnessDown>" . (lambda () (interactive)
                                              (start-process "bri" nil "brightnessctl" "set" "5%-")))
               ("<XF86AudioRaiseVolume>"  . (lambda () (interactive)
                                              (start-process "vol" nil "wpctl" "set-volume" "@DEFAULT_AUDIO_SINK@" "5%+")))
               ("<XF86AudioLowerVolume>"  . (lambda () (interactive)
                                              (start-process "vol" nil "wpctl" "set-volume" "@DEFAULT_AUDIO_SINK@" "5%-")))
               ("<XF86AudioMute>"         . (lambda () (interactive)
                                              (start-process "vol" nil "wpctl" "set-mute" "@DEFAULT_AUDIO_SINK@" "toggle"))))
      '';
    };
  };
}
```

  **Note on the `setopt` interpolation:** write it so Nix interpolates `config.programs.ewmOutputConfig` into the elisp string. Concretely, in the actual file use Nix string interpolation:

```nix
        config = ''
          (setopt ewm-output-config ${config.programs.ewmOutputConfig})
        '';
```

  (The host sets `ewmOutputConfig` to a quoted alist like `''(("DP-2" :width 3840 ...))''` or leaves it `"nil"`.)

- [ ] **Step 5: Import the ewm Emacs module.** In `modules/home-manager/profiles/wayland/default.nix`, add to (or create) an `imports` list: `./emacs-ewm.nix`.

- [ ] **Step 6: Build the pgtk Emacs in isolation.** Temporarily enable wayland on satori's HM to type-check (revert after): easier — build the finalPackage directly once a host is switched (Task 8). For now, verify non-wayland Linux still builds with pgtk:

Run: `nixos-rebuild build --flake '.#satori' 2>&1 | tail -8`
Expected: builds (satori still on desktop profile; Emacs is now pgtk, which runs fine under X). If org-latex-preview / native-comp / heavy packages break under pgtk, fix before proceeding (they are display-backend agnostic; failures would be unrelated build issues).

- [ ] **Step 7: Checkpoint.** `nix fmt`; `git add` the three files. Proposed message: `profiles/wayland: pgtk Emacs + ewm window-manager config`.

---

### Task 5: waybar status bar (home-manager)

**Files:**
- Create: `modules/home-manager/profiles/wayland/waybar.nix`
- Modify: `modules/home-manager/profiles/wayland/default.nix` (import)

**Interfaces:**
- Consumes: stylix colors `config.lib.stylix.colors.withHashtag`, host-specific module list via `programs.waybarModulesRight` option (Tasks 8–10 override per host, mirroring the old polybar `modules-right`).
- Produces: a layer-shell waybar with an SNI tray, porting polybar's cpu/memory/temperature/network/battery/pulseaudio/clock/tray.

- [ ] **Step 1: Create the waybar module.** Create `modules/home-manager/profiles/wayland/waybar.nix`:

```nix
{
  config,
  lib,
  pkgs,
  ...
}:
let
  colors = config.lib.stylix.colors.withHashtag;
in
{
  options.programs.waybarModulesRight = lib.mkOption {
    type = lib.types.listOf lib.types.str;
    default = [
      "cpu"
      "memory"
      "network"
      "pulseaudio"
      "tray"
    ];
    description = "waybar modules-right list (host-overridable).";
  };

  config = lib.mkIf (config.profiles.wayland.enable && pkgs.stdenv.isLinux) {
    programs.waybar = {
      enable = true;
      systemd.enable = true;
      settings.mainBar = {
        layer = "top";
        position = "top";
        height = 28;
        modules-left = [ ];
        modules-center = [ "clock" ];
        modules-right = config.programs.waybarModulesRight;

        clock = {
          format = " {:%Y-%m-%d %a  %H:%M:%S}";
          interval = 1;
          tooltip-format = "<tt>{calendar}</tt>";
        };
        cpu = {
          interval = 2;
          format = "  {usage}%";
          on-click = "${lib.getExe pkgs.ghostty} -e ${lib.getExe pkgs.btop}";
        };
        memory = {
          interval = 2;
          format = "  {percentage}%";
          on-click = "${lib.getExe pkgs.ghostty} -e ${lib.getExe pkgs.btop}";
        };
        temperature = {
          format = " {temperatureC}°C";
          critical-threshold = 80;
        };
        network = {
          interval = 2;
          format-ethernet = "󰛳  {ifname} {bandwidthDownBits}";
          format-wifi = "  {essid} {signalStrength}%";
          format-disconnected = "󰲛 disconnected";
        };
        battery = {
          states = {
            warning = 30;
            critical = 10;
          };
          format = "  {capacity}%";
          format-charging = "  {capacity}%";
        };
        pulseaudio = {
          format = "󰕾 {volume}%";
          format-muted = "󰝟";
          on-click = "${lib.getExe pkgs.pavucontrol}";
          scroll-step = 5;
        };
        tray = {
          icon-size = 18;
          spacing = 8;
        };
      };
      style = ''
        * { font-family: "${config.stylix.fonts.monospace.name}"; font-size: ${toString config.stylix.fonts.sizes.desktop}pt; }
        window#waybar { background: ${colors.base00}; color: ${colors.base05}; }
        #clock, #cpu, #memory, #temperature, #network, #battery, #pulseaudio, #tray { padding: 0 8px; }
        #battery.warning { color: ${colors.base09}; }
        #battery.critical { color: ${colors.base08}; }
        #temperature.critical { color: ${colors.base08}; }
        /* base06 desktop accent */
        #clock { color: ${colors.base06}; }
      '';
    };
  };
}
```

- [ ] **Step 2: Import it.** Add `./waybar.nix` to `imports` in `modules/home-manager/profiles/wayland/default.nix`.

- [ ] **Step 3: Verify eval** (full build happens at host cutover, Task 8).

Run: `nix eval '.#nixosConfigurations.satori.config.home-manager.users.matt.programs.waybarModulesRight' 2>&1 | tail -3`
Expected: prints the default list (option present).

- [ ] **Step 4: Checkpoint.** `nix fmt`; `git add` the two files. Proposed message: `profiles/wayland: waybar status bar + tray`.

---

### Task 6: Notifications, lock, idle, wallpaper, screenshots, gamma (home-manager)

**Files:**
- Create: `modules/home-manager/profiles/wayland/services.nix`
- Modify: `modules/home-manager/profiles/wayland/default.nix` (import + ghostty/packages/xdg moved here)

**Interfaces:**
- Produces: mako (notifications), swaylock+swayidle (lock/idle), swaybg (wallpaper), grim/slurp/satty (screenshots), wl-clipboard, gammastep (wayland), plus the ghostty/chromium/xdg config carried over from the old desktop profile (Linux bits).

- [ ] **Step 1: Create the services module.** Create `modules/home-manager/profiles/wayland/services.nix`:

```nix
{
  config,
  lib,
  pkgs,
  ...
}:
let
  colors = config.lib.stylix.colors.withHashtag;
in
{
  config = lib.mkIf (config.profiles.wayland.enable && pkgs.stdenv.isLinux) {

    home.packages = with pkgs; [
      wl-clipboard
      grim
      slurp
      satty
      pavucontrol
      thunar
      swaybg
    ];

    services.mako = {
      enable = true;
      settings = {
        anchor = "top-right";
        border-size = 6;
        border-color = colors.base06; # base06 accent
        background-color = colors.base00;
        text-color = colors.base05;
        width = 600;
        margin = "12";
        default-timeout = 8000;
      };
    };

    services.swayidle = {
      enable = true;
      timeouts = [
        {
          timeout = 600;
          command = "${lib.getExe pkgs.swaylock} -f";
        }
      ];
    };

    programs.swaylock = {
      enable = true;
      settings.color = lib.removePrefix "#" colors.base00;
    };

    # Wallpaper: ewm may set this natively; swaybg is the portable fallback.
    systemd.user.services.swaybg = {
      Unit = {
        Description = "Wallpaper via swaybg";
        PartOf = [ "graphical-session.target" ];
      };
      Install.WantedBy = [ "graphical-session.target" ];
      Service.ExecStart = "${lib.getExe pkgs.swaybg} -m fill -i ${config.stylix.image}";
    };

    services.gammastep = {
      enable = true;
      provider = "geoclue2";
      settings.general.brightness-night = 0.6;
      # NOTE: gammastep uses wlr-gamma-control on Wayland. If ewm/Smithay does
      # not implement it, this is a no-op; see plan risk note. Verify on satori.
    };
  };
}
```

- [ ] **Step 2: Move the carried-over Linux desktop config into the wayland profile.** In `modules/home-manager/profiles/wayland/default.nix`, add the ghostty config, chromium, mpv, zathura, udiskie (tray), and the `xdg` block (org-protocol desktop entry + mimeApps) from the **old** `modules/home-manager/profiles/desktop/default.nix` — but **omit** the X11-only pieces: `xclip`, `feh`, `urxvt`, `services.picom`, `services.polybar`, `services.flameshot`, `services.dunst`, and the `xsession`/`windowManager.xmonad` block. Replace `feh.desktop` image-handler defaults in `mimeApps` with an available viewer (e.g. keep image types pointed at chromium or `imv.desktop` if added). Add `./services.nix` and `./emacs-ewm.nix` and `./waybar.nix` to this file's `imports`.

  (Concretely: copy the `programs.ghostty`, `programs.chromium`, `programs.mpv`, `programs.zathura`, `services.udiskie`, and `xdg` attributes verbatim; drop the rest. The ghostty block is platform-shared but the wayland profile is Linux-gated, so keep its Linux branch.)

- [ ] **Step 3: Verify eval.**

Run: `nix eval '.#nixosConfigurations.satori.config.home-manager.users.matt.services.mako.enable' 2>&1 | tail -3`
Expected: `false` (satori not yet on wayland) with no eval error.

- [ ] **Step 4: Checkpoint.** `nix fmt`; `git add` the files. Proposed message: `profiles/wayland: notifications, lock, idle, wallpaper, screenshots`.

---

### Task 7: Stylix Wayland targets

**Files:**
- Create: `modules/stylix/wayland.nix`
- Modify: `flake.nix` (`makeNixosSystem` modules: import `./modules/stylix/wayland.nix`)

**Interfaces:**
- Produces: stylix targets enabled for mako/swaylock/cursor under Wayland; keeps the base16 scheme. X11 cursor/wallpaper targets in `modules/stylix/linux.nix` remain valid (cursor applies to both).

- [ ] **Step 1: Inspect available stylix targets** to use correct names:

Run: `nix eval --json '.#nixosConfigurations.satori.options.stylix.targets' 2>/dev/null | tr ',' '\n' | grep -iE 'mako|sway|waybar|gtk|cursor' | head`
Expected: lists target attribute names (e.g. `mako`, `swaylock`). Use the real names in Step 2.

- [ ] **Step 2: Create `modules/stylix/wayland.nix`** (home-manager-side targets are auto-applied by stylix; this module documents/forces any that need it). Minimal version — rely on stylix autodetection, only force where a target is off by default:

```nix
{ lib, ... }:
{
  # Stylix auto-themes mako/swaylock/gtk when those programs are enabled.
  # Keep this module as the seam for any Wayland-specific overrides.
  # (Intentionally minimal; waybar/mako colors are set explicitly in their
  # own modules to preserve the base06 desktop-accent convention.)
}
```

  If Step 1 shows a needed target defaults off, enable it here under `stylix.targets.<name>.enable = true;`.

- [ ] **Step 3: Verify** `nixos-rebuild build --flake '.#satori' 2>&1 | tail -5` still builds.

- [ ] **Step 4: Checkpoint.** `nix fmt`; `git add modules/stylix/wayland.nix flake.nix`. Proposed message: `stylix: wayland target seam`.

---

### Task 8: Cut satori over to wayland (build only; user verifies on hardware)

**Files:**
- Modify: `flake.nix` (satori: `profiles.desktop.enable` → `profiles.wayland.enable` on both nixos + hm sides; set `renderDevice`)
- Modify: `hosts/satori/configuration/default.nix` (remove `services.xserver`)
- Rewrite: `hosts/satori/home/default.nix` (replace polybar/rofi/Xft DPI with ewm output config + waybar modules)

**Interfaces:**
- Consumes: `programs.ewmOutputConfig`, `programs.waybarModulesRight` (Tasks 4–5), `programs.ewm.renderDevice` (Task 1 module).

- [ ] **Step 1: Flip satori's profiles in `flake.nix`.** In the `satori` block change:

```nix
              profiles = {
                cuda.enable = true;
                wayland.enable = true;
                personal.enable = true;
                nvidia.enable = true;
              };

              home-manager.users.matt.profiles = {
                wayland.enable = true;
                distrobox.enable = true;
                nvidia.enable = true;
                personal.enable = true;
              };
```

  Also set the NVIDIA render device and ensure modesetting (nvidia profile already sets `hardware.nvidia.modesetting.enable = true`). Add to the satori inline module:

```nix
              programs.ewm.renderDevice = "/dev/dri/renderD128";
              boot.kernelParams = [ "nvidia-drm.modeset=1" ];
```

  (Confirm the render node in Step 4; `renderD128` is the usual single-GPU default.)

- [ ] **Step 2: Remove X server from satori config.** In `hosts/satori/configuration/default.nix`, delete the entire `services.xserver = { … xrandrHeads … };` block. Keep `console.keyMap = "us"`. (Wayland keyboard layout `us` is the default; if a non-default layout were needed it would go via `services.xserver.xkb` which greetd/ewm read, but `us` needs nothing.)

- [ ] **Step 3: Rewrite `hosts/satori/home/default.nix`** to the wayland equivalents (outputs DP-2 primary + DP-0; nvidia-gpu + temperature in waybar). Replace the whole file with:

```nix
{ ... }:
{
  config = {
    home.stateVersion = "25.05";

    programs.spotify.package = null; # set in private/personal if needed

    # Two 4K displays side by side (DP-2 left/primary, DP-0 right).
    # scale 1.5 ~= the old Xft.dpi 163 perceptual size.
    programs.ewmOutputConfig = ''
      '(("DP-2" :width 3840 :height 2160 :scale 1.5 :x 0 :y 0)
        ("DP-0" :width 3840 :height 2160 :scale 1.5 :x 2560 :y 0))
    '';

    programs.waybarModulesRight = [
      "network"
      "cpu"
      "memory"
      "temperature"
      "pulseaudio"
      "tray"
    ];
  };
}
```

  Note: keep the existing `programs.spotify.package` override if desired (the old value `pkgs.spotify.override { deviceScaleFactor = 1.8; }` still works under Wayland — preserve it rather than nulling; the `null` above is only a placeholder to flag review). Confirm real monitor resolutions with the user (3840×2160 assumed; the old config didn't record resolution, only outputs).

- [ ] **Step 4: Build and discover hardware facts.**

Run: `nixos-rebuild build --flake '.#satori' 2>&1 | tail -15`
Expected: builds. If the Emacs finalPackage fails to find the ewm elisp, revisit Task 4 Step 1.

Run (on satori): `ls -l /dev/dri/by-path/ | grep render; ls /sys/class/drm/*/device/uevent | head` and `cat /sys/class/hwmon/*/name` to confirm render node + temperature hwmon. Adjust `renderDevice` / waybar `temperature.hwmon-path` if needed.

- [ ] **Step 5: USER HARDWARE VERIFICATION (not the implementer).** Hand off to the user with these checks: `sudo nixos-rebuild switch --flake '.#satori'`; reboot or `systemctl restart greetd`; at tuigreet pick the user, log in to ewm; verify: both outputs come up at correct resolution/scale; `s-d` launches/switches; a Wayland app (ghostty) opens as a buffer; waybar shows with tray; mako notification (`notify-send test`); screenshot (`grim`); brightness/volume keys; gammastep at night (or note it's a no-op — see risk). Report back before proceeding to golem.

- [ ] **Step 6: Checkpoint.** `nix fmt`; `git add flake.nix hosts/satori/configuration/default.nix hosts/satori/home/default.nix`. Proposed message: `satori: migrate to ewm wayland desktop`.

---

### Task 9: Cut golem over to wayland (build only; user verifies)

**Files:**
- Modify: `flake.nix` (golem profiles + renderDevice + kernelParams)
- Modify: `hosts/golem/configuration/default.nix` (remove `services.xserver`)
- Rewrite: `hosts/golem/home/default.nix`

- [ ] **Step 1: Flip golem profiles** in `flake.nix` exactly as Task 8 Step 1 (golem has `cuda`, `desktop`, `personal`, `nvidia` → swap `desktop`→`wayland` on both nixos + hm sides; golem hm currently has `desktop, nvidia, personal`). Add `programs.ewm.renderDevice = "/dev/dri/renderD128";` and `boot.kernelParams = [ "nvidia-drm.modeset=1" ];`. golem uses `hardware.nvidia.open = false` (proprietary) — modesetting still applies.

- [ ] **Step 2: Remove `services.xserver`** (incl. the `DP-2` `Rotate left` `monitorConfig`) from `hosts/golem/configuration/default.nix`.

- [ ] **Step 3: Rewrite `hosts/golem/home/default.nix`** with rotation via `:transform 1` (90°) on DP-2:

```nix
{ ... }:
{
  config = {
    home.stateVersion = "21.11";

    programs.ewmOutputConfig = ''
      '(("DP-4" :width 3840 :height 2160 :scale 1.5 :x 0 :y 0)
        ("DP-2" :width 3840 :height 2160 :scale 1.5 :transform 1 :x 2560 :y 0))
    '';

    programs.waybarModulesRight = [
      "network"
      "cpu"
      "memory"
      "temperature"
      "pulseaudio"
      "tray"
    ];
  };
}
```

  Confirm resolutions/positions with the user; `:transform 1` = 90° (the old config rotated DP-2 left). Preserve golem's `programs.spotify.package` override if wanted.

- [ ] **Step 4: Build.** `nixos-rebuild build --flake '.#golem' 2>&1 | tail -15`. Expected: builds.

- [ ] **Step 5: USER HARDWARE VERIFICATION** (golem deploys via nixos-config-private input repointing — Ref `project_satori_branch_deploy`/`project_karakuri_deploy_from_private`). Same checklist as Task 8 Step 5, plus confirm DP-2 rotation. Report back.

- [ ] **Step 6: Checkpoint.** `nix fmt`; stage the three files. Proposed message: `golem: migrate to ewm wayland desktop`.

---

### Task 10: Cut karakuri over to wayland (build only; user verifies)

**Files:**
- Modify: `flake.nix` (karakuri profiles)
- Modify: `hosts/karakuri/configuration/default.nix` (remove `services.xserver`, `services.autorandr`)
- Rewrite: `hosts/karakuri/home/default.nix`

- [ ] **Step 1: Flip karakuri profiles** (`desktop`→`wayland` on both sides; karakuri has `desktop, personal`). No NVIDIA (Intel/AMD), so no `renderDevice`/`nvidia-drm.modeset` needed; ewm auto-selects the single GPU.

- [ ] **Step 2: Remove from `hosts/karakuri/configuration/default.nix`:** the `services.xserver` block and the `services.autorandr` block (dock hotplug is handled by ewm output config). Keep `hardware.acpilight.enable`, libinput, i2c/ddcutil, blueman. Keep `stylix.fonts.sizes` override.

- [ ] **Step 3: Rewrite `hosts/karakuri/home/default.nix`** (laptop panel eDP-1 + external when docked; HiDPI scale ≈ old DPI 224 → scale 2.0). External output name unknown until inspected; provide eDP-1 plus a commented dock example:

```nix
{ ... }:
{
  home.stateVersion = "21.11";

  # ThinkPad X1 internal panel. scale 2.0 ~= the old Xft.dpi 224.
  # Docked external outputs hotplug in; add entries once names are known
  # (wlr-randr / ewm reports them, e.g. "DP-3").
  programs.ewmOutputConfig = ''
    '(("eDP-1" :width 3840 :height 2400 :scale 2.0 :x 0 :y 0))
  '';

  programs.waybarModulesRight = [
    "wireless-network"
    "cpu"
    "memory"
    "battery"
    "pulseaudio"
    "tray"
  ];
}
```

  Note: waybar uses one `network` module (auto-detects wifi vs ethernet); the list above uses `network` semantics — use `"network"` rather than `"wireless-network"` (waybar has no `wireless-network`). Correct the list to `[ "network" "cpu" "memory" "battery" "pulseaudio" "tray" ]`. Confirm panel resolution (3840×2400 assumed for X1) with the user.

- [ ] **Step 4: Build.** `nixos-rebuild build --flake '.#karakuri' 2>&1 | tail -15`. Expected: builds.

- [ ] **Step 5: USER HARDWARE VERIFICATION** — same checklist; plus battery module, dock hotplug (plug in, confirm external output appears; capture its connector name and add to `ewmOutputConfig`), brightness via acpilight. Report back.

- [ ] **Step 6: Checkpoint.** `nix fmt`; stage the three files. Proposed message: `karakuri: migrate to ewm wayland desktop`.

---

### Task 11: Delete the X11/xmonad stack and rename wayland → desktop

**Files:**
- Delete: `modules/home-manager/profiles/desktop/xmonad.nix`, `modules/home-manager/profiles/desktop/xmonad/` (dir), `modules/home-manager/profiles/desktop/polybar.nix`, `modules/home-manager/profiles/desktop/rofi.nix`
- Delete/empty: `modules/nixos/profiles/desktop/default.nix` X11 bits
- Modify: `modules/home-manager/profiles/nvidia.nix` (drop the polybar `nvidia-gpu` block; the waybar equivalent lives in the wayland profile)
- Rename: `profiles.wayland` → `profiles.desktop` across the tree (option name, host wiring, gates)

**Interfaces:**
- Precondition: Tasks 8–10 all verified on hardware by the user. Do NOT start until the user confirms all three hosts are green on ewm.

- [ ] **Step 1: Confirm green light.** Verify with the user that satori, golem, and karakuri all run ewm successfully. (This task removes the fallback.)

- [ ] **Step 2: Fold the nvidia-gpu indicator into waybar.** The old `modules/home-manager/profiles/nvidia.nix` adds a polybar `nvidia-gpu` script module. Replace it with a waybar `custom/nvidia` module gated on `config.profiles.wayland.enable` (or, post-rename, `profiles.desktop.enable`). Add to `waybar.nix` a `"custom/nvidia"` module and reference `nvidia-smi`; remove the polybar block from `nvidia.nix`. Concrete waybar module:

```nix
        "custom/nvidia" = {
          interval = 2;
          exec = "${pkgs.nvtopPackages.nvidia}/bin/nvidia-smi --query-gpu=utilization.gpu --format=csv,noheader,nounits | ${pkgs.gawk}/bin/awk '{printf \"GPU %2d%%\", $1}'";
          on-click = "${lib.getExe pkgs.ghostty} -e ${lib.getExe pkgs.nvtopPackages.nvidia}";
        };
```

  (Hosts that list `nvidia-gpu` in `programs.waybarModulesRight` should use `custom/nvidia`; update satori/golem lists accordingly.)

- [ ] **Step 3: Delete X11 modules.** Remove the files listed above. In `modules/home-manager/profiles/desktop/default.nix` (old) — this whole file is being replaced by the renamed wayland profile, so delete the old desktop directory's X11 content. Remove `./polybar.nix ./rofi.nix ./xmonad.nix` from any `imports`.

- [ ] **Step 4: Strip NixOS desktop X11.** Either delete `modules/nixos/profiles/desktop/` entirely (it only held xserver/lightdm/xterm + pipewire import) and move `pipewire.nix` under the renamed profile, or empty its `config`. Simplest: delete the old `modules/nixos/profiles/desktop/` and have the renamed wayland profile import pipewire from its new location.

- [ ] **Step 5: Rename the profile.** Rename `modules/{nixos,home-manager}/profiles/wayland` → `.../profiles/desktop`; change every `profiles.wayland` to `profiles.desktop` (option declarations, `lib.mkIf` gates in `emacs-ewm.nix`, `waybar.nix`, `services.nix`, `base/emacs/default.nix` pgtk/frames-only gates) and update `profiles/default.nix` imports. In `flake.nix` change all `wayland.enable` back to `desktop.enable` for satori/golem/karakuri.

Run a sweep: `grep -rn 'profiles.wayland\|profiles/wayland\|wayland.enable' modules flake.nix hosts` — expected: empty after the rename.

- [ ] **Step 6: Full build of all three hosts.**

Run: `for h in satori golem karakuri; do echo "== $h =="; nixos-rebuild build --flake ".#$h" 2>&1 | tail -3; done`
Expected: all three build.

Run: `nix flake check 2>&1 | tail -15`
Expected: passes (no Haskell sources left for hlint/ormolu to choke on — they now lint nothing relevant; confirm no errors).

- [ ] **Step 7: USER FINAL VERIFICATION.** User does `nixos-rebuild switch` on satori and redeploys golem/karakuri to confirm the renamed profile is functionally identical. Then the migration is complete.

- [ ] **Step 8: Checkpoint.** `nix fmt`; stage all deletions/renames. Proposed message: `desktop: remove X11/xmonad stack, rename wayland profile → desktop`. (Risky/large; per repo convention this is its own commit — Ref `CLAUDE.md` "Risky or potentially-reverted changes go in their own commit".)

---

## Self-Review

**Spec coverage:**
- §1 scope / full rip-out → Tasks 8–11. ✓
- §2 architecture (pgtk, ewm module, drop frames-only-mode, one Emacs) → Tasks 3–4. ✓
- §3 parallel profile + staged rollout + rename → Tasks 2, 8–11. ✓
- §4 NixOS layer (flake input, ewm module, greetd, removals) → Tasks 1, 3, 8–10. ✓
- §5 home-manager (emacs, waybar, mako, lock/idle, wallpaper, screenshots, clipboard, gammastep, launcher) → Tasks 4–6. ✓
- §6 per-host outputs/rotation/scale/renderDevice → Tasks 8–10. ✓
- §7 stylix wayland targets → Task 7. ✓
- §8 risks (NVIDIA, gammastep gamma, ewm maturity, pgtk) → called out in Tasks 4, 6, 8 + Global Constraints. ✓
- §9 testing/rollback → build-only deliverable + user cutover, fallback kept until Task 11. ✓

**Placeholder scan:** The only deliberately-deferred facts are hardware values that cannot be known without the machine (render node, hwmon path, exact monitor resolutions/connector names, the ewm elisp attr path) — each has an explicit discovery command and a fallback, not a bare "TODO". The `programs.spotify.package = null` in Task 8 Step 3 is flagged in-step as a placeholder to confirm, not a final value.

**Type consistency:** `programs.ewmOutputConfig` (lines option), `programs.waybarModulesRight` (listOf str), `programs.emacs.ewmEmacsPackage` (package), `programs.ewm.{enable,emacsPackage,renderDevice,screencast.enable}` (from the module) are used consistently across Tasks 3–11. waybar module-name correction (`network`, not `wireless-network`) is noted in Tasks 5/10.

**Open items for the codex review to scrutinize:**
1. The ewm elisp sourcing (Task 4 Step 1) — is `config.programs.ewm.ewmPackage` an Emacs-loadable package, and is reading it back via `config.osConfig` in home-manager sound?
2. `programs.ewm.emacsPackage = config.home-manager.users.matt.programs.emacs.finalPackage` — circular-reference risk between the NixOS ewm module and the HM Emacs (HM reads `osConfig.programs.ewm.ewmPackage`, NixOS reads HM `finalPackage`). May need to source the elisp directly from `pkgs.ewm`/the input instead of via `osConfig` to break the cycle.
3. greetd `--cmd ewm` session name correctness.
4. Whether `programs.emacs.extraPackages` is the correct rycee `emacs-init` attribute (vs `extraEmacsPackages`).

---

## Revisions after codex review (AUTHORITATIVE — override the task text above where they conflict)

Codex reviewed this plan and the repo (2026-06-27). All findings verified against the
tree. The following revisions are authoritative; where they contradict the original task
text, follow the revision.

**R1 (Critical — breaks the module cycle). Task 4: source the ewm elisp from the flake
input, not `osConfig`.** Drop the `programs.emacs.ewmEmacsPackage` option and the
`config.osConfig.programs.ewm.ewmPackage` read entirely. Instead, in
`modules/home-manager/profiles/base/emacs/default.nix`, add `inputs` to the module args and
source the elisp directly. **VERIFIED (2026-06-27):** ewm's `nix/default.nix` returns
`emacsPackage.pkgs.trivialBuild { pname = "ewm"; packageRequires = [ ewm-core ]; }` — an
Emacs package carrying `ewm.el` + the compositor `.so`. Only `packages.<sys>.default` is
exposed, and it has `.override`. Build it against the SAME base Emacs as our package set
(`emacs-unstable-pgtk`, R9), otherwise byte-code version skew (the flake default builds
against `emacs-pgtk` 30.2):

```nix
    programs.emacs.extraPackages =
      epkgs:
      lib.optionals (pkgs.stdenv.isLinux && config.profiles.wayland.enable) [
        epkgs.vterm
        (inputs.ewm.packages.${pkgs.stdenv.hostPlatform.system}.default.override {
          emacsPackage = pkgs.emacs-unstable-pgtk;
        })
      ];
```

  `emacs-ewm.nix` keeps ONLY the `ewm` use-package (bindings + `ewm-output-config`) and the
  `ewmOutputConfig` option; it no longer defines `ewmEmacsPackage`. Task 3 keeps
  `programs.ewm.emacsPackage = config.home-manager.users.matt.programs.emacs.finalPackage;`.
  No cycle: the elisp override depends on the base `emacs-unstable-pgtk`, not on
  `finalPackage`; the module's internal `ewmPackage` (built against `cfg.emacsPackage.emacs`,
  the base) is lazy and unused by the launch script.

**R2 (Critical — RESOLVED 2026-06-27).** Verified by reading
`${inputs.ewm}/nix/{default,service.nix}` from the locked store path and eval-checking
`inputs.ewm.packages.x86_64-linux.default.override { emacsPackage = pkgs.emacs-unstable-pgtk; }`
→ valid `emacs-ewm-0.1.0.drv`. The `.override` form in R1 is the confirmed source. No
further discovery step needed.

**R3 (Critical). Task 2: PipeWire is inert under the wayland profile.**
`modules/nixos/profiles/desktop/pipewire.nix` is gated on `config.profiles.desktop.enable`.
Generalize its gate so the wayland profile activates audio. Edit that file:

```nix
  config = lib.mkIf (config.profiles.desktop.enable || config.profiles.wayland.enable) {
```

  (Do this as a step in Task 2, since the wayland profile imports `../desktop/pipewire.nix`.)

**R4 (Critical). Task 4: Emacs daemon collision.**
`modules/home-manager/profiles/base/default.nix:243` enables `services.emacs` (Linux),
which would race ewm's own `emacs --fg-daemon` on the server socket. Gate it off under
wayland. Change that block to:

```nix
        services.emacs = lib.mkIf (!(pkgs.stdenv.isLinux && config.profiles.wayland.enable)) {
          enable = true;
          client.enable = true;
        };
```

  (Requires `config`/`pkgs` already in scope there — they are.)

**R5 (Critical). Task 3: greetd session selection.** `--cmd ewm` is wrong (it's a literal
command, not a session selector). Point tuigreet at the registered wayland-sessions dir and
use `pkgs.tuigreet`:

```nix
    services.greetd = {
      enable = true;
      settings.default_session = {
        command = "${lib.getExe pkgs.tuigreet} --time --remember --remember-session --asterisks --sessions ${config.services.displayManager.sessionData.desktops}/share/wayland-sessions";
        user = "greeter";
      };
    };
```

  Verify the registered session by inspecting `…/share/wayland-sessions/ewm.desktop`
  (`Name=`, `Exec=`) once Task 8 builds satori.

**R6 (Critical). Tasks 8/9/10: `programs.spotify.package = null` fails eval** (type is
`package`). Do NOT null it. Preserve each host's existing override verbatim, e.g. satori:
`programs.spotify.package = pkgs.spotify.override { deviceScaleFactor = 1.8; };` (so each
rewritten `hosts/<h>/home/default.nix` must take `pkgs` in its args again).

**R7 (Critical — external repo). Private repo re-enables the old profile.**
`~/projects/nixos-config-private/flake.nix` sets `profiles.desktop.enable = true` for golem
(matt), karakuri (kiso), and satori (matt **and** kiso). When the public host flips to
`profiles.wayland`, the private overlay would enable BOTH stacks. During the migration
window, the private repo must, for each migrated host, set
`profiles.desktop.enable = lib.mkForce false;` and `profiles.wayland.enable = true;` on both
the nixos and per-user (`home-manager.users.{matt,kiso}.profiles`) sides. This is a separate
repo and a separate change — flag to the user; do not assume it's done.

**R8 (Critical — design caveat surfaced by R7). Multi-user ewm.** The NixOS-level
`programs.ewm.emacsPackage` is a single binary/package set for the whole session, but the
private repo gives satori/karakuri a second user `kiso` with her own Emacs. The public repo
is matt-only, so `config.home-manager.users.matt.programs.emacs.finalPackage` is correct
here. The private repo must decide how kiso's ewm session gets her packages (her own
`finalPackage`, or accept the primary user's package set on the load path with her own
init.el). Out of scope for the public repo; documented for the private follow-up.

**R9 (Should-fix). Task 4: use `pkgs.emacs-unstable-pgtk`, not `pkgs.emacs-pgtk`.** Current
Linux Emacs is `emacs-unstable` (31.x, from emacs-overlay); bare `emacs-pgtk` is 30.2.
`emacs-unstable-pgtk` comes from the already-applied emacs-overlay. Use:

```nix
      package =
        if pkgs.stdenv.isDarwin then pkgs.emacs-macport else pkgs.emacs-unstable-pgtk;
```

  Verify it resolves: `nix eval '.#nixosConfigurations.satori.config.home-manager.users.matt.programs.emacs.package.version'` after the edit.

**R10 (Should-fix). Task 11: NVIDIA waybar `nvidia-smi` path.** `pkgs.nvtopPackages.nvidia`
does not provide `nvidia-smi`. Mirror the existing polybar code and use the driver package:

```nix
        "custom/nvidia" = {
          interval = 2;
          exec = "${osConfig.hardware.nvidia.package.bin}/bin/nvidia-smi --query-gpu=utilization.gpu --format=csv,noheader,nounits | ${pkgs.gawk}/bin/awk '{printf \"GPU %2d%%\", $1}'";
          on-click = "${lib.getExe pkgs.ghostty} -e ${lib.getExe pkgs.nvtopPackages.nvidia}";
        };
```

  (`waybar.nix` must then take `osConfig` in its args; the nvidia indicator belongs in the
  nvidia-gated path, mirroring today's `modules/home-manager/profiles/nvidia.nix`.)

**R11 (Should-fix). Tasks 8/9: NVIDIA + Smithay session env.** Beyond
`hardware.nvidia.modesetting.enable` (already set by the nvidia profile), Smithay on NVIDIA
commonly needs `GBM_BACKEND=nvidia-drm` and `__GLX_VENDOR_LIBRARY_NAME=nvidia`. If the ewm
module doesn't set them, add for satori/golem (NixOS):

```nix
      environment.sessionVariables = {
        GBM_BACKEND = "nvidia-drm";
        __GLX_VENDOR_LIBRARY_NAME = "nvidia";
      };
```

  Treat as a hardware-verification knob (Task 8 Step 5): try without first, add if ewm fails
  to start on NVIDIA.

**R12 (RESOLVED 2026-06-27 — no change needed).** Read `${inputs.ewm}/resources/ewm.service`:
it is `Type=notify` with `BindsTo=graphical-session.target` and `Before=graphical-session.target`.
Starting `ewm.service` (which `ewm-session` does) activates `graphical-session.target`, so
home-manager user services bound to it (waybar via `systemd.enable = true`, the `swaybg`
unit, swayidle) start correctly. Keep `programs.waybar.systemd.enable = true`. The session
name is `ewm` and the desktop `Exec=ewm-session`, confirming R5's `--sessions` approach.

**Confirmed fine (no change):** rycee `programs.emacs.extraPackages` is the correct option;
`frames-only-mode` gating via `enable = false` is structurally sound.
