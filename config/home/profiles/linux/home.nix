{ pkgs, ... }: {
  imports = [ ../../home.nix ./org-notes-sync.nix ];

  home.packages = with pkgs; [ anki signal-desktop zathura ];

  nixpkgs.config.packageOverrides = pkgs: {
    nur = import (builtins.fetchTarball
      "https://github.com/nix-community/NUR/archive/master.tar.gz") {
        inherit pkgs;
      };
  };

  programs = {
    chromium.enable = true;

    firefox = {
      enable = true;

      extensions = with pkgs.nur.repos.rycee.firefox-addons; [
        browserpass
        https-everywhere
        privacy-badger
        ublock-origin
        vimium
      ];
    };

    git.ignores = pkgs.ghGitIgnoreLines "Global/Linux";

    password-store.package =
      pkgs.pass.withExtensions (exts: with exts; [ pass-update pass-otp ]);
  };

  services = {
    gpg-agent = {
      enable = true;
      defaultCacheTtl = 14400; # 4 hours
      maxCacheTtl = 14400;
    };

    lorri.enable = true;

    org-notes-sync = {
      enable = true;
      repoPath = "${builtins.getEnv "HOME"}/src/org-notes/";
      frequency = "*:0/5";
    };

    password-store-sync.enable = true;

    random-background = {
      enable = true;
      imageDirectory = "%h/.background-images";
    };
  };

  xdg = {
    configFile.xmobar.source = "${pkgs.mcwitt-dotfiles}/config/xmobar/";

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
}
