{
  config,
  lib,
  options,
  pkgs,
  ...
}:
{
  options.profiles.wayland.enable = lib.mkEnableOption "Profile for machines running the ewm (Wayland) desktop";

  # Audio is a separate orthogonal component: hosts enable profiles.audio
  # alongside this profile.

  config = lib.mkIf config.profiles.wayland.enable (
    lib.mkMerge [
      {
        # required for `gtk.enable = true` in home-manager
        programs.dconf.enable = true;

        # Minimal TTY greeter. The ewm module registers a `share/wayland-sessions/
        # ewm.desktop` (session name "ewm", Exec=ewm-session) via
        # services.displayManager.sessionPackages; point tuigreet at that directory.
        services.greetd = {
          enable = true;
          settings.default_session = {
            command = lib.concatStringsSep " " [
              (lib.getExe pkgs.tuigreet)
              "--time"
              "--remember"
              "--remember-session"
              "--asterisks"
              "--sessions ${config.services.displayManager.sessionData.desktops}/share/wayland-sessions"
            ];
            user = "greeter";
          };
        };

        # Mature fallback session in case ewm (young, master-tracked) breaks.
        # Registers a second wayland-session next to ewm; pick at the tuigreet
        # prompt (--remember-session keeps ewm the default). The shared shell
        # (waybar, mako, swayidle, swaybg) binds to graphical-session.target,
        # which niri also starts, so it works unchanged there.
        programs.niri.enable = true;

        environment.systemPackages = with pkgs; [
          wl-clipboard # ewm copy/paste between Emacs and Wayland apps
          brightnessctl # media-key brightness control
          # ewm spawns this by name from PATH when an X11 client connects
          # (niri-style on-demand XWayland); without it X11 apps (Steam,
          # Proton games) cannot start. The ewm module does not ship it.
          xwayland-satellite
          # niri's auto-generated default config binds Mod+T -> alacritty and
          # Mod+D -> fuzzel (hotkey overlay on first start shows the rest);
          # ship both so the stock fallback session is usable without a
          # hand-maintained niri config.
          alacritty
          fuzzel
        ];

        # swaylock authenticates via PAM; without this /etc/pam.d entry every
        # unlock attempt is denied (the ewm module does not import nixpkgs'
        # wayland-session.nix, which normally provides it for sway/hyprland).
        security.pam.services.swaylock = { };
      }

      # `programs.ewm` is declared only on hosts that import the ewm NixOS module
      # (the makeNixosSystem desktop hosts: satori/golem/karakuri). Headless hosts
      # (hal/hestia/hob) share this profile module but lack that option, so guard
      # with optionalAttrs: unlike mkIf it omits the attribute entirely when the
      # option is absent, avoiding an unmatched-definition error (which would also
      # eagerly evaluate the home-manager reference on hosts without home-manager).
      (lib.optionalAttrs (options.programs ? ewm) {
        programs.ewm = {
          enable = true;
          # One unified Emacs: the compositor host IS the user's full daily-driver
          # Emacs (pgtk + ewm elisp + vterm), built by home-manager. The ewm launch
          # script runs `${emacsPackage}/bin/emacs --fg-daemon --eval (require 'ewm)`,
          # so the package must carry the ewm elisp (added in the base emacs module).
          emacsPackage = config.home-manager.users.matt.programs.emacs.finalPackage;
          screencast.enable = true;
        };
      })
    ]
  );
}
