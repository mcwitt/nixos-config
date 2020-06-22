{ pkgs, ... }: {
  imports = [ ../../home.nix ./emacs.nix ./org-notes-sync.nix ];

  home.packages = with pkgs; [
    anki
    dmenu
    haskellPackages.xmobar
    signal-desktop
    zathura
  ];

  nixpkgs.config.packageOverrides = pkgs: {
    nur = import (builtins.fetchTarball
      "https://github.com/nix-community/NUR/archive/master.tar.gz") {
        inherit pkgs;
      };
  };

  programs.chromium.enable = true;

  programs.firefox = {
    enable = true;

    extensions = with pkgs.nur.repos.rycee.firefox-addons; [
      browserpass
      https-everywhere
      privacy-badger
      ublock-origin
      vimium
    ];
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
        "application/pdf" =
          [ "${pkgs.zathura}/share/applications/org.pwmt.zathura.desktop" ];

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
