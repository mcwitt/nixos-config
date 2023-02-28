{ config, lib, pkgs, ... }:
{
  imports = [
    ./rofi.nix
    ./xmonad.nix
  ];

  home.packages = with pkgs; [
    libnotify
    mplayer
    signal-desktop
    spotify
    xfce.thunar
  ];

  home.pointerCursor = {
    package = pkgs.vanilla-dmz;
    name = "Vanilla-DMZ";
    size = lib.mkDefault 48;
    x11.enable = true;
  };

  programs.chromium = {
    enable = true;
    package = pkgs.chromium.override {
      # enable Chromecast
      commandLineArgs = "--load-media-router-component-extension=1";
    };
    extensions =
      let
        browserpass = "naepdomgkenhinolocfifgehidddafch";
        dark-reader = "eimadpbcbfnmbkopoojfekhnkhdbieeh";
        link-to-text-fragment = "pbcodcjpfjdpcineamnnmbkkmkdpajjg";
        privacy-badger = "pkehgijcmpdhfbdbbnkijodmdjhbjlgp";
        ublock-origin = "cjpalhdlnbpafiamejdnhcphjbkeiagm";
        vimium = "dbepggeogbaibhgnhhndojpepiihcmeb";
      in
      [
        browserpass
        dark-reader
        link-to-text-fragment
        privacy-badger
        ublock-origin
        vimium
      ];
  };

  programs.emacs.org-protocol.enable = true;

  programs.feh.enable = true;

  programs.firefox = {
    enable = true;
    extensions = with pkgs.nur.repos.rycee.firefox-addons; [
      browserpass
      darkreader
      privacy-badger
      ublock-origin
      vimium
    ];
    profiles.default.settings = {
      "network.protocol-handler.expose.org-protocol" = true;
    };
  };

  programs.git.ignores = lib.gitignores "Global/Linux";

  programs.urxvt = {
    enable = true;
    fonts = [ "xft:Iosevka Comfy:size=10:antialias=true" ];
  };

  programs.vscode.package = pkgs.vscodium-fhs;

  programs.zathura.enable = true;

  services.dunst = {
    enable = true;
    settings = {
      global = {
        browser = "${config.programs.chromium.package}/bin/chromium-browser";
        font = "Iosevka Comfy 10";
        markup = "full";
        max_icon_size = 100;
        text_icon_padding = 10;
      };
      urgency_low = {
        background = "#002b36";
        foreground = "#93a1a1";
        timeout = 5;
      };
      urgency_normal = {
        background = "#002b36";
        foreground = "#859900";
        timeout = 10;
      };
      urgency_critical = {
        background = "#002b36";
        foreground = "#dc322f";
        timeout = 20;
      };
    };
  };

  services.emacs = {
    enable = true;
    client.enable = true;
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

  services.gpg-agent = {
    enable = true;
    defaultCacheTtl = 14400; # 4 hours
    maxCacheTtl = 14400;
  };

  services.org-notes-sync = {
    enable = true;
    repoPath = "${config.home.homeDirectory}/src/org-notes/";
    frequency = "*:0/5";
  };

  services.pass-secret-service.enable = true;

  services.password-store-sync.enable = true;

  services.random-background = {
    enable = true;
    imageDirectory = "%h/.background-images";
  };

  services.stalonetray = {
    enable = true;
    config = {
      background = config.scheme.withHashtag.base00;
      kludges = "force_icons_size";
      transparent = false;
    };
  };

  home.shellAliases.open = "${pkgs.xdg-utils}/bin/xdg-open";

  xdg = {
    enable = true;
    mimeApps = {
      enable = true;
      defaultApplications =
        let
          mkDefaults = apps: types: builtins.listToAttrs
            (map (type: lib.nameValuePair type apps) types);
        in
        mkDefaults [ "feh.desktop" ] [
          "image/bmp"
          "image/gif"
          "image/jpeg"
          "image/jpg"
          "image/png"
          "image/webp"
        ] //
        mkDefaults [ "chromium-browser.desktop" ] [
          "text/html"
          "x-scheme-handler/http"
          "x-scheme-handler/https"
          "x-scheme-handler/ftp"
        ] // {
          "application/pdf" = [ "org.pwmt.zathura.desktop" ];
          "text/plain" = [ "emacsclient.desktop" ];
        };
    };
  };

  xsession.enable = true;
}
