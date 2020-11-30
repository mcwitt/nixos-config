{ config, pkgs, ... }: {
  imports = [ ./alacritty.nix ];

  home.packages = with pkgs; [
    anki
    dmenu
    libnotify
    peek
    signal-desktop
    slack
    steam
    xmobar
    zoom-us
    zulip
  ];

  home.sessionVariables.EDITOR =
    "${config.programs.emacs.finalPackage}/bin/emacsclient --tty";

  profiles.personal.enable = true;

  programs.chromium = {
    enable = true;
    extensions = [
      "naepdomgkenhinolocfifgehidddafch" # Browserpass
      "eimadpbcbfnmbkopoojfekhnkhdbieeh" # Dark Reader
      "gcbommkclmclpchllfjekcdonpmejbdp" # HTTPS Everywhere
      "pkehgijcmpdhfbdbbnkijodmdjhbjlgp" # Privacy Badger
      "cjpalhdlnbpafiamejdnhcphjbkeiagm" # uBlock Origin
      "dbepggeogbaibhgnhhndojpepiihcmeb" # Vimium
    ];
  };

  programs.emacs.org-protocol.enable = true;

  programs.feh.enable = true;

  programs.firefox = {
    enable = true;
    extensions = with pkgs.nur.repos.rycee.firefox-addons; [
      browserpass
      darkreader
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

  programs.zathura.enable = true;

  services.dunst.enable = true;

  services.emacs.enable = true;

  services.flameshot.enable = true;

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

  shells.aliases.open = "${pkgs.xdg_utils}/bin/xdg-open";

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
        "text/html" = [ "chromium-browser.desktop" ];
        "x-scheme-handler/http" = [ "chromium-browser.desktop" ];
        "x-scheme-handler/https" = [ "chromium-browser.desktop" ];
        "x-scheme-handler/ftp" = [ "chromium-browser.desktop" ];
        "x-scheme-handler/about" = [ "chromium-browser.desktop" ];
        "x-scheme-handler/unknown" = [ "chromium-browser.desktop" ];
      };
    };
  };

  xresources.extraConfig =
    builtins.readFile (pkgs.mypkgs.sources.solarized + "/xresources/solarized");

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
      config = "${pkgs.mypkgs.sources.xmonad-config}/xmonad.hs";
    };
  };
}
