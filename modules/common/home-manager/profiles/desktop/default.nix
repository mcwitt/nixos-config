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
  options.profiles.desktop.enable = lib.mkEnableOption "Profile for machines with graphical desktops";

  imports = [ ./firefox.nix ];

  config = lib.mkIf cfg.enable {

    home.packages = [ pkgs.xclip ];

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
          mkDefaults [ "feh.desktop" ] [
            "image/bmp"
            "image/gif"
            "image/jpeg"
            "image/jpg"
            "image/png"
            "image/webp"
          ]
          // mkDefaults [ "chromium-browser.desktop" ] [
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
  };
}
