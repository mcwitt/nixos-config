{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.profiles.desktop;
in
{
  options.profiles.desktop.enable = lib.mkEnableOption "Profile for headed (graphical) machines";

  imports = [
    ./firefox.nix
    ./polybar.nix
    ./rofi.nix
    ./xmonad.nix
  ];

  config = lib.mkIf cfg.enable (
    lib.mkMerge [
      {
        programs.ghostty = {
          enable = true;

          # pkgs.ghostty is Linux-only; on darwin the app is installed via a
          # Homebrew cask (see nixos-config-private) and we only write the
          # shared config. Theming is handled by the stylix ghostty target.
          package = lib.mkIf pkgs.stdenv.isDarwin null;

          settings = {
            window-show-tab-bar = "auto"; # zellij owns tabs; hide the lone one
            auto-update = "off"; # updates come from nix / Homebrew
            clipboard-read = "allow";
            clipboard-write = "allow"; # let emacs clipetty OSC-52 through
          }
          // lib.optionalAttrs pkgs.stdenv.isDarwin {
            # macOS default shell is zsh/bash; force a fish login shell,
            # matching the old wezterm default_prog.
            command = "${lib.getExe config.programs.fish.package} -l";
          };
        };

        stylix.enable = true;
      }

      (lib.mkIf pkgs.stdenv.isLinux {
        home.packages = [
          pkgs.pavucontrol
          pkgs.xclip
          pkgs.thunar
        ];

        # Land local terminals in the persistent `main` session. The
        # home-manager zellij shell integration only emits a bare
        # `zellij attach -c` (no name), and zellij does not read a
        # ZELLIJ_SESSION_NAME env var, so attach to `main` explicitly instead.
        # Scoped to desktop + Linux so it never fires on the MacBook (which
        # reaches zellij over SSH).
        programs.fish.interactiveShellInit = ''
          if status is-interactive; and not set -q ZELLIJ
              ${lib.getExe config.programs.zellij.package} attach --create main
          end
        '';
        programs.zsh.initContent = lib.mkOrder 200 ''
          if [[ -o interactive && -z "$ZELLIJ" ]]; then
            ${lib.getExe config.programs.zellij.package} attach --create main
          fi
        '';

        programs.chromium = {
          enable = true;
          extensions =
            let
              bitwarden = "nngceckbapebfimnlniiiahkandclblb";
              link-to-text-fragment = "pbcodcjpfjdpcineamnnmbkkmkdpajjg";
              privacy-badger = "pkehgijcmpdhfbdbbnkijodmdjhbjlgp";
              vimium = "dbepggeogbaibhgnhhndojpepiihcmeb";
            in
            [
              bitwarden
              link-to-text-fragment
              privacy-badger
              vimium
            ];
        };

        programs.feh.enable = true;

        programs.mpv.enable = true;

        programs.urxvt = {
          enable = true;
          fonts =
            let
              inherit (config.stylix) fonts;
            in
            [ "xft:${fonts.monospace.name}:size=${toString fonts.sizes.applications}:antialias=true" ];
        };

        programs.vscodium = {

          enable = true;

          package = pkgs.vscodium-fhs;

          profiles.default = {

            extensions = with pkgs.vscode-extensions; [
              arrterian.nix-env-selector
              ms-toolsai.jupyter
              vscodevim.vim
            ];

            keybindings = [
              {
                key = "alt+n";
                command = "editor.action.marker.nextInFiles";
                when = "editorFocus";
              }
              {
                key = "alt+p";
                command = "editor.action.marker.prevInFiles";
                when = "editorFocus";
              }
            ];

            userSettings = {

              update.mode = "none";
              extensions.autoUpdate = false;

              editor = {
                fontFamily = "'${config.stylix.fonts.monospace.name}'";
                fontLigatures = true;
                formatOnSave = true;
              };

              vim = {
                hlsearch = true;
                insertModeKeyBindings = [
                  {
                    before = [
                      "f"
                      "d"
                    ];
                    after = [ "<Esc>" ];
                  }
                ];
              };
            };
          };
        };

        programs.zathura.enable = true;

        services.dunst = {
          enable = true;
          settings =
            let
              colors = config.lib.stylix.colors.withHashtag;
            in
            {
              global = {
                browser = "${config.programs.chromium.package}/bin/chromium-browser";
                markup = "full";
                max_icon_size = 100;
                text_icon_padding = 10;
                scale = 1;
                frame_width = 6;
                origin = "top-right";
                offset = "12x58";
                width = 600;
              };

              urgency_low.foreground = lib.mkForce colors.base04;

              urgency_normal = {
                frame_color = lib.mkForce colors.base06;
                highlight = lib.mkForce colors.base06;
                foreground = lib.mkForce colors.base06;
              };

              urgency_critical.foreground = lib.mkForce colors.base08;
            };
        };

        services.flameshot = {
          enable = true;
          settings.General.showStartupLaunchMessage = false;
        };

        services.gammastep = {
          enable = true;
          tray = true;
          provider = "geoclue2";
          settings.general = {
            adjustment-method = "randr";
            brightness-night = 0.6;
          };
        };

        services.picom = {
          enable = true;
          backend = "glx";
          vSync = true;
          shadow = true;
          settings = {
            crop-shadow-to-monitor = true;
            corner-radius = 6;
            rounded-corners-exclude = [ "window_type = 'dock'" ];
          };
        };

        services.polybar.enable = true;

        services.udiskie = {
          enable = true;
          tray = "always";
        };

        xdg = {
          enable = true;

          desktopEntries.org-protocol = {
            name = "org-protocol";
            comment = "Intercept calls from emacsclient to trigger custom actions";
            categories = [ "X-Other" ];
            icon = "emacs";
            type = "Application";
            exec = "emacsclient -- %u";
            terminal = false;
            mimeType = [ "x-scheme-handler/org-protocol" ];
          };

          mimeApps = {
            enable = true;
            defaultApplications =
              let
                mkDefaults = apps: types: builtins.listToAttrs (map (type: lib.nameValuePair type apps) types);
              in
              mkDefaults
                [ "feh.desktop" ]
                [
                  "image/bmp"
                  "image/gif"
                  "image/jpeg"
                  "image/jpg"
                  "image/png"
                  "image/webp"
                ]
              //
                mkDefaults
                  [ "chromium-browser.desktop" ]
                  [
                    "text/html"
                    "x-scheme-handler/http"
                    "x-scheme-handler/https"
                    "x-scheme-handler/ftp"
                  ]
              // {
                "application/pdf" = [ "org.pwmt.zathura.desktop" ];
                "text/plain" = [ "emacsclient.desktop" ];
              };
          };
        };

        xsession = {
          enable = true;
          windowManager.xmonad.enable = true;
        };
      })
    ]
  );
}
