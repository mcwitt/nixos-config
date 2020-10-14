{ config, pkgs, lib, ... }:

let
  emacs = config.programs.emacs.finalPackage;

  orgProtocolDesktopItem = pkgs.makeDesktopItem rec {
    name = "org-protocol";
    desktopName = name;
    mimeType = "x-scheme-handler/org-protocol";
    exec = "${emacs}/bin/emacsclient %u";
    icon = "emacs";
    type = "Application";
    terminal = "false";
    categories = "System";
  };

  shellAliases = {
    # override aliases to use home-manager emacs
    emacs = lib.mkForce "${emacs}/bin/emacsclient --create-frame";
    ec = lib.mkForce "${emacs}/bin/emacsclient --tty";

    open = "${pkgs.xdg_utils}/bin/xdg-open";
  };

in {
  imports = [ ../../home.nix ./alacritty.nix ./org-notes-sync.nix ];

  home.packages = with pkgs; [
    anki
    dmenu
    factorio
    libnotify
    orgProtocolDesktopItem
    signal-desktop
    slack
    steam
    xmobar
    zoom-us
    zulip
  ];

  home.sessionVariables.EDITOR = "${emacs}/bin/emacsclient --tty";

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

  programs.emacs = {
    enable = true;
    package = pkgs.emacsGit;
    extraPackages = pkgs.mypkgs.emacsPackages;
  };

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

  programs.fish.shellAliases = shellAliases;

  programs.git.ignores = pkgs.mypkgs.gitignore.ghGitIgnoreLines "Global/Linux";

  programs.password-store.package =
    pkgs.pass.withExtensions (exts: with exts; [ pass-update pass-otp ]);

  programs.zathura.enable = true;

  programs.zsh.shellAliases = shellAliases;

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
