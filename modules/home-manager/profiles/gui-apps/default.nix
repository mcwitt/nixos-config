{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.profiles.gui-apps;
in
{
  options.profiles.gui-apps.enable = lib.mkEnableOption "Shared GUI applications (session-agnostic)";

  imports = [ ./firefox.nix ];

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
            shell-integration-features = "ssh-env,ssh-terminfo";
            # ghostty defaults to 2pt; give the text room to breathe.
            window-padding-x = 8;
            window-padding-y = 8;
            mouse-scroll-multiplier = "discrete:0.75";
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
          pkgs.imv # image viewer (X11 + Wayland backends)
          pkgs.pavucontrol
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

        programs.mpv.enable = true;

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
                [ "imv.desktop" ]
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
      })
    ]
  );
}
