{ config, pkgs, ... }:

let
  orgProtocolDesktopItem = pkgs.makeDesktopItem rec {
    name = "org-protocol";
    desktopName = name;
    mimeType = "x-scheme-handler/org-protocol";
    exec = "${config.programs.emacs.finalPackage}/bin/emacsclient %u";
    icon = "emacs";
    type = "Application";
    terminal = "false";
    categories = "System";
  };

in {
  imports = [ ../../home.nix ./org-notes-sync.nix ];

  home.packages = with pkgs; [
    anki
    dmenu
    haskellPackages.xmobar
    orgProtocolDesktopItem
    signal-desktop
  ];

  nixpkgs.config.packageOverrides = pkgs: {
    nur = import (builtins.fetchTarball
      "https://github.com/nix-community/NUR/archive/master.tar.gz") {
        inherit pkgs;
      };
  };

  programs.chromium.enable = true;

  programs.emacs = {
    enable = true;
    extraPackages = pkgs.mypkgs.emacsPackages;
  };

  programs.feh.enable = true;

  programs.firefox = {
    enable = true;
    extensions = with pkgs.nur.repos.rycee.firefox-addons; [
      browserpass
      https-everywhere
      privacy-badger
      ublock-origin
      vimium
    ];
    profiles.default.settings = {
      "network.protocol-handler.expose.org-protocol" = true;
    };
  };

  programs.git.ignores = pkgs.mypkgs.gitignore.ghGitIgnoreLines "Global/Linux";

  programs.password-store.package =
    pkgs.pass.withExtensions (exts: with exts; [ pass-update pass-otp ]);

  programs.urxvt = {
    enable = true;
    extraConfig = {
      # clickable URLs
      perl-ext-common = "default,matcher";
      url-launcher = "${pkgs.xdg_utils}/bin/xdg-open";
      "matcher.button" = 1;
    };
    fonts = [ "xft:Fira Code:size=11" ];
    scroll.bar.enable = false;
  };

  programs.zathura.enable = true;

  services.emacs = {
    enable = true;
    client.enable = true;
  };

  services.gpg-agent = {
    enable = true;
    defaultCacheTtl = 14400; # 4 hours
    maxCacheTtl = 14400;
  };

  services.lorri.enable = true;

  services.org-notes-sync = {
    enable = true;
    repoPath = "${builtins.getEnv "HOME"}/src/org-notes/";
    frequency = "*:0/5";
  };

  services.password-store-sync.enable = true;

  services.random-background = {
    enable = true;
    imageDirectory = "%h/.background-images";
  };

  xdg = {
    enable = true;
    configFile.xmobar.source = "${pkgs.mypkgs.dotfiles}/config/xmobar/";
    mimeApps = {
      enable = true;
      defaultApplications = {
        "application/pdf" = [ "org.pwmt.zathura.desktop" ];
        "image/bmp" = [ "feh.desktop" ];
        "image/gif" = [ "feh.desktop" ];
        "image/jpeg" = [ "feh.desktop" ];
        "image/jpg" = [ "feh.desktop" ];
        "image/pjpeg" = [ "feh.desktop" ];
        "image/png" = [ "feh.desktop" ];
        "image/tiff" = [ "feh.desktop" ];
        "image/webp" = [ "feh.desktop" ];
        "image/x-bmp" = [ "feh.desktop" ];
        "image/x-pcx" = [ "feh.desktop" ];
        "image/x-png" = [ "feh.desktop" ];
        "image/x-portable-anymap" = [ "feh.desktop" ];
        "image/x-portable-bitmap" = [ "feh.desktop" ];
        "image/x-portable-graymap" = [ "feh.desktop" ];
        "image/x-portable-pixmap" = [ "feh.desktop" ];
        "image/x-tga" = [ "feh.desktop" ];
        "image/x-xbitmap" = [ "feh.desktop" ];
        "text/html" = [ "firefox.desktop" ];
        "x-scheme-handler/http" = [ "firefox.desktop" ];
        "x-scheme-handler/https" = [ "firefox.desktop" ];
        "x-scheme-handler/ftp" = [ "firefox.desktop" ];
        "x-scheme-handler/about" = [ "firefox.desktop" ];
        "x-scheme-handler/unknown" = [ "firefox.desktop" ];
      };
    };
  };

  xresources.extraConfig = builtins.readFile (pkgs.fetchFromGitHub {
    owner = "altercation";
    repo = "solarized";
    rev = "62f656a02f93c5190a8753159e34b385588d5ff3";
    sha256 = "0001mz5v3a8zvi3gzmxhi3yrsb6hs7qf6i497arsngnvj2cwn61d";
  } + "/xresources/solarized");

  xsession = {
    enable = true;

    pointerCursor = {
      package = pkgs.vanilla-dmz;
      name = "Vanilla-DMZ";
      size = 48;
    };

    windowManager.xmonad = {
      enable = true;
      enableContribAndExtras = true;

      config = pkgs.writeText "xmonad.hs" ''
        import XMonad
        import XMonad.Hooks.DynamicLog
        import XMonad.Layout.NoBorders

        -- modify default layout hook with 'smartBorders'
        myLayoutHook = smartBorders $ layoutHook def

        main = xmonad =<< xmobar def
          { borderWidth        = 5
          , normalBorderColor  = "#073642"
          , focusedBorderColor = "#859900"
          , layoutHook         = myLayoutHook
          , modMask            = mod4Mask
          , terminal           = "urxvt"
          }
      '';
    };
  };
}
