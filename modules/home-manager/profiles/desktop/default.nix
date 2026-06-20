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

          # nixpkgs ghostty is Linux-only; darwin installs the app via Homebrew
          # (see nixos-config-private), so write config only.
          package = lib.mkIf pkgs.stdenv.isDarwin null;

          settings = {
            window-show-tab-bar = "auto";
            auto-update = "off";
            clipboard-read = "allow";
            clipboard-write = "allow"; # for emacs clipetty (OSC-52)
            # ghostty defaults to 2pt; give the text room to breathe.
            window-padding-x = 8;
            window-padding-y = 8;
          }
          // lib.optionalAttrs pkgs.stdenv.isLinux {
            window-decoration = "none";
          }
          // lib.optionalAttrs pkgs.stdenv.isDarwin {
            command = "${lib.getExe config.programs.fish.package} -l"; # macOS defaults to zsh
            # stylix scales the terminal font by 4/3 on darwin, rendering larger
            # than every other app; pin to the unscaled size.
            font-size = lib.mkForce config.stylix.fonts.sizes.terminal;
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
            corner-radius = 8;
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
