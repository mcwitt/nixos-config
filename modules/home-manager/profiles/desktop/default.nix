{
  config,
  inputs,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.profiles.desktop;
in
{
  options.profiles.desktop.enable = lib.mkEnableOption "Profile for machines with graphical desktops";

  imports = [
    inputs.peon-ping.homeManagerModules.default
    ./firefox.nix
    ./polybar.nix
    ./rofi.nix
    ./xmonad.nix
  ];

  config = lib.mkIf cfg.enable {

    home.packages = [
      pkgs.pavucontrol
      pkgs.xclip
      pkgs.thunar
      inputs.peon-ping.packages.${pkgs.stdenv.hostPlatform.system}.default
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

    programs.peon-ping = {
      enable = true;
      package = inputs.peon-ping.packages.${pkgs.stdenv.hostPlatform.system}.default;
      installPacks = [
        "peon"
      ];
      settings = {
        default_pack = "peon";
        enabled = true;
        volume = 1.0;
        desktop_notifications = false;
      };
      enableBashIntegration = false;
      enableZshIntegration = false;
    };

    programs.wezterm = {
      enable = true;

      colorSchemes.custom = with config.lib.stylix.colors.withHashtag; {
        # https://github.com/chriskempson/base16-shell/blob/master/templates/default.mustache
        ansi = [
          base00
          red
          green
          yellow
          blue
          magenta
          cyan
          base05
        ];
        brights = [
          base03
          red
          green
          yellow
          blue
          magenta
          cyan
          base07
        ];

        background = base00;
        cursor_bg = base05;
        cursor_border = base05;
        cursor_fg = base00;
        foreground = base05;
        selection_bg = base05;
        selection_fg = base00;
      };
      extraConfig =
        let
          inherit (config.stylix) fonts;
        in
        ''
          return {
            font = wezterm.font "${fonts.monospace.name}",
            font_size = ${toString fonts.sizes.applications},
            color_scheme = "custom",
            hide_tab_bar_if_only_one_tab = true,
            check_for_updates = false,
            audible_bell = "Disabled",
            keys = {{key="Enter", mods="SHIFT", action=wezterm.action{SendString="\x1b\r"}}},
          };
        '';
    };

    programs.urxvt = {
      enable = true;
      fonts =
        let
          inherit (config.stylix) fonts;
        in
        [ "xft:${fonts.monospace.name}:size=${toString fonts.sizes.applications}:antialias=true" ];
    };

    programs.vscode = {

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
      settings.global = {
        browser = "${config.programs.chromium.package}/bin/chromium-browser";
        markup = "full";
        max_icon_size = 100;
        text_icon_padding = 10;
      };
    };

    services.flameshot = {
      enable = true;
      settings.General = {
        showStartupLaunchMessage = false;
        useX11LegacyScreenshot = true;
      };
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

    services.gpg-agent = {
      pinentry.package = pkgs.pinentry-gtk2;

      # TODO: pinentry-rofi breaks in ssh sessions
      # extraConfig =
      #   let
      #     pinentry-rofi-with-env = pkgs.writeShellApplication {
      #       name = "pinentry-rofi-with-env";
      #       runtimeInputs = with pkgs; [ coreutils rofi ];
      #       text = ''
      #         "${pkgs.pinentry-rofi}/bin/pinentry-rofi" "$@"
      #       '';
      #     };
      #   in
      #   ''
      #     pinentry-program ${pinentry-rofi-with-env}/bin/pinentry-rofi-with-env
      #   '';
    };

    services.picom = {
      enable = false; # XXX
      backend = "glx";
      activeOpacity = 1.0;
      inactiveOpacity = 0.9;
      fade = true;
      fadeDelta = 3;
      shadow = true;
      settings = {
        blur = {
          method = "gaussian";
          size = 10;
          deviation = 5.0;
        };
        blur-background-exclude = [
          "class_g ?= 'zoom'"
        ];
      };
    };

    services.polybar.enable = true;

    services.udiskie = {
      enable = true;
      tray = "always";
    };

    stylix.enable = true;

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
  };
}
